module Pyrec.Check where

import           Prelude                 hiding (map, mapM, sequence)

import           Control.Applicative
import           Control.Monad           hiding (mapM, sequence)
import           Control.Monad.Reader    hiding (mapM, sequence)

import           Data.List                      (find)
import qualified Data.Map          as M
import           Data.Map                       (Map)
import           Data.Maybe                     (fromMaybe)
import           Data.Traversable        hiding (for)

import           Pyrec.Misc

import qualified Pyrec.IR          as IR        (Pattern)
import           Pyrec.IR                hiding (Pattern)
import           Pyrec.IR.Desugar  as D
import qualified Pyrec.IR.Check    as C

type Pattern = IR.Pattern D.BindT BindN
type Entry   = Decl D.BindT D.BindN ()
type Env     = Map Id Entry
type CK      = Reader Env

emptyEnv :: Env
emptyEnv = M.empty

fixType :: Env -> D.Expr -> D.Type -> C.Expr
fixType env (D.E l et e) t = tc env $ D.E l (unify env t et) e

tc :: Env -> D.Expr -> C.Expr
tc env (D.E l t e) = case e of

  Num n -> se (unify env t $ D.T TNum) $ Num n
  Str s -> se (unify env t $ D.T TStr) $ Str s

  Ident i -> case M.lookup i env of
    Nothing -> se t $ Ident $ C.Unbound i
    Just (Def dt (BT l _ t') _) -> e'
      where i' = C.Bound dt l i
            e' = se (unify env t t') $ Ident i'
    Just (Data (BN l _) _) -> e' -- type-as-term case which shouldn't occur in practice
      where i' = C.Bound Val l i
            e' = se (unify env t $ D.T TType) $ Ident i'

  Let (Def kind (BT vl vi vt) v) e -> C.E l t' $ Let (newDef v') e'
    where v'@(C.E _ vt' _) = fixType env v vt
          newDef q         = Def kind (BT vl vi vt') q
          env'             = M.insert vi (newDef ()) env
          e'@(C.E _ t'  _) = fixType env' e t

  Let (Data i@(BN _ bi) variants) e -> se t' $ Let data' e'
    where envData :: Entry -- phantom ex type parameter nessesitates rebuild
          envData = Data i variants

          fixVar :: Variant D.BindT BindN -> Variant D.BindT BindN
          fixVar (Variant vi args) = Variant vi $ (fmap . fmap) fixField args
            where fixField :: D.BindT -> D.BindT
                  fixField (BT bl bi t) = BT bl bi $ checkT (M.insert bi envData env) t

          variants' = fixVar <$> variants

          bindConstrs :: Variant D.BindT BindN -> (Id , Entry)
          bindConstrs (Variant (BN vl vi) ps) = (vi, Def Val (BT vl vi $ T $ k $ TIdent vi) ())
            where k = case ps of
                    Nothing     -> id
                    Just params -> TFun (for params $ \(BT _ _ t) -> t) . T

          env'             = M.union (M.fromList $ (bi, envData) : fmap bindConstrs variants') env
          e'@(C.E _ t'  _) = fixType env' e t

          data'            = Data i variants'

  Assign i v -> se t'' $ Assign i' v' -- immutability errors caught downstream in Pyrec.Report
     where (i', t') = case M.lookup i env of
             Nothing                     -> (C.Unbound     i , t)
             Just (Def dt (BT l _ t') _) -> (C.Bound dt  l i , t')
             Just (Data   (BN l _)    _) -> (C.Bound Val l i , t')

           v'@(C.E _ t'' _) = fixType env v t'

  Fun params body -> se t' $ Fun params' body'
    where params'                 = for params $ \(BT l i t) -> BT l i $ checkT env t
          bindParams b@(BT _ i _) = (i, Def Val b ())
          env'                    = M.union (M.fromList $ bindParams <$> params') env
          body'@(C.E _ retT _)    = tc env' body
          t'                      = unify env t $ D.T
                                    $ TFun (for params' $ \(BT _ _ pt) -> pt) retT

  FunT params body -> se t' $ FunT params body'
    where bindParams (BN l i)     = (i, Def Val (BT l i $ D.T TType) ())
          env'                    = M.union (M.fromList $ bindParams <$> params) env
          body'@(C.E _ retT _)    = tc env' body
          t'                      = unify env t $ D.T $ TParam params retT

  App f args -> se t' $ App f' args'
    where f'@(C.E _ ft' _) = fixType env f ft
          args' = tc env <$> args
          getT (C.E _ t _) = t
          ft = D.T $ TFun (getT <$> args') t
          t' = case ft' of
            D.T (TFun _ retT) -> retT
            _                 -> D.TError $ D.TypeMismatch ft ft'

  AppT f args -> se t' $ AppT f' args'
    where f'@(C.E _ ft' _) = fixType env f ft
          args' = checkT env <$> args
          ft = D.T $ TFun args' t
          t' = case ft' of
            D.T (TParam _ retT) -> retT
            _                   -> D.TError $ D.TypeMismatch ft ft'

  Cases vt v cases -> se t'' $ Cases vt' v' cases'
    where v'@(C.E _ vt' _) = fixType env v vt

          bind :: D.Type -> Pattern -> (Bool , Pattern , Env)
          bind t p = case p of

            (Binding (BT l i t')) -> ( True
                                     , Binding $ BT l i t''
                                     , M.singleton i $ Def Val (BT l i t'') ())
              where t'' = unify env t t'

            (Constr ci pats) -> result
              where ensureIdent (T (TIdent i)) = Just i
                    ensureIdent _              = Nothing

                    -- outer maybe signifies error, inner maybe is use to distinguish
                    -- pyret's "| Constructor()" vs "| Constructor"
                    params :: Maybe (Maybe [D.Type])
                    params = do (Variant _ params) <-
                                  ensureIdent t
                                  >>= (\i -> M.lookup i env)
                                  >>= (\x -> case x of
                                          Data _ vs -> Just vs
                                          _         -> Nothing)

                                  >>= find (\(Variant vi _) -> vi == ci)
                                guard $ (length <$> params) == (length <$> pats)
                                return $ flip (fmap . fmap) params $ \(BT _ _ t) -> t

                    results :: Maybe [(Bool, Pattern , Env)]
                    (newerr, results) = case params of
                      Nothing     -> (False , fmap fmap fmap (bind $ T TAny) pats)
                      Just params -> (True  , zipWith bind <$> params <*> pats)

                    result = case results of
                      Nothing   -> ( newerr
                                   , Constr ci Nothing
                                   , M.empty)
                      Just list -> ( and $ newerr : errs
                                   , Constr ci $ Just pats
                                   , M.unions envs)
                        where (errs, pats, envs) = unzip3 list

          checkCase :: (Bool, D.Type)  -> Case D.BindT BindN D.Expr ->
                       ((Bool , D.Type) , Case D.BindT BindN C.Expr)
          checkCase (err, t) (Case pat body) = ((err && err' , t') , Case pat' body')
            where (err' , pat' , caseEnv) = bind vt' pat
                  body'@(C.E _ t'  _)     = fixType caseEnv body t

          ((err , t') , cases') = mapAccumL checkCase (True, t) cases

          t'' = if err then TError $ D.CantCaseAnalyze t' else t'

  EmptyObject -> se (unify env t $ D.T $ TObject M.empty) EmptyObject

  -- can this be made less rediculous?
  Extend obj fi fv -> se t' $ Extend obj' fi fv'
    where fv' @(C.E _ ft  _) = tc env fv

          ot                 = case t of
            D.T (TObject o)   -> D.T $ TObject $ trim o
            D.PartialObj p    -> D.PartialObj  $ trim p
            _                 -> D.PartialObj $ M.empty
          trim map           = M.delete fi map

          obj'@(C.E _ ot' _) = fixType env obj ot

          t'                 = unify env t $ case ot' of
            D.T (TObject o)   -> D.T $ TObject $ tryRefine o
            D.PartialObj p    -> D.PartialObj  $ tryRefine p
            _                 -> D.PartialObj  $ M.singleton fi ft
          tryRefine map      = M.insert fi ft map

  Access obj fi -> se t'' $ Access obj' fi
    where t'                 = D.PartialObj $ M.singleton fi t
          obj'@(C.E _ ot  _) = fixType env obj t'
          t''                = unify env t $ case ot of
            D.T (TObject o)   -> tryRefine o
            D.PartialObj p    -> tryRefine p
            _                 -> t'
          tryRefine map      = fromMaybe t' $ M.lookup fi map

  where se = C.E l


checkT :: Env -> D.Type -> D.Type
checkT env t = case t of
  TUnknown            -> TUnknown
  D.T TNum            -> t
  D.T TStr            -> t
  D.T (TFun args res) -> D.T $ TFun (checkT env <$> args) res

-- expected then got
unify :: Env -> D.Type -> D.Type -> D.Type
unify env a b = case (a, b) of
  (D.T TNum,   D.T TNum)   -> D.T TNum
  (D.T TStr,   D.T TStr)   -> D.T TStr

  (D.T (TFun aParams aRes), D.T (TFun bParams bRes)) ->
    try $ D.T <$> do
      params <- map2S recur aParams bParams
      return $ TFun params $ recur aRes bRes

  ((D.T (TObject o1)), (D.T (TObject o2))) ->
    try $ D.T <$> TObject <$> do
      guard $ M.size o1 == M.size o2             -- same size
      let match = M.intersectionWith recur o1 o2 -- unify common elements
      guard $ M.size match == M.size o1          -- intersection same size means ident keys
      return match

  (l@(D.T (TObject _)), r@(D.PartialObj _)) ->
    recur r l                                    -- generative recursion
  (D.PartialObj p, D.T (TObject o)) ->
    try $ D.T <$> TObject <$> do
      guard $ M.isSubmapOfBy (\_ _ -> True) p o  -- is partial subset
      let update = M.intersectionWith recur o p  -- unify fields in common
      return $ M.union update o                  -- perform updates

  (D.TUnknown, other)      -> checkT env other
  (other,      D.TUnknown) -> checkT env other

  (_,          _)          -> TError $ TypeMismatch a b

  where recur = unify env
        try :: Maybe D.Type -> D.Type
        try t = case t of
          Just t  -> t
          Nothing -> TError $ TypeMismatch a b
