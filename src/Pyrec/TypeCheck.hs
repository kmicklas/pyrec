{-# LANGUAGE StandaloneDeriving #-}

module Pyrec.TypeCheck where

import           Prelude                 hiding (mapM, sequence)

import           Control.Applicative
import           Control.Monad           hiding (mapM, sequence)
import           Control.Monad.Reader    hiding (mapM, sequence)

import           Data.List                      (foldl', find)
import qualified Data.Map          as M
import           Data.Map                       (Map)
import           Data.Maybe                     (fromMaybe)
import           Data.Traversable        hiding (for)

import           Pyrec.Misc
import           Pyrec.Error

import qualified Pyrec.IR          as IR        (Pattern)
import           Pyrec.IR                hiding (Pattern)
import qualified Pyrec.IR.Desugar  as D
import           Pyrec.IR.Desugar               (BindN(..))
import           Pyrec.IR.Check    as C

deriving instance Ord BindN

type Pattern = IR.Pattern C.BindT BindN
type Entry   = Decl BindT BindN ()
type Env     = Map BindN Entry
type CK      = Reader Env

extendMap :: Map BindN a -> Map BindN a -> Map BindN a
extendMap = flip M.union

emptyEnv :: Env
emptyEnv = M.empty

bindNParam :: BindN -> (BindN, Entry)
bindNParam k@(BN l i) = (k, Def Val (BT l i $ C.T TType) ())

extendN = uncurry M.insert . bindNParam

bindTParam :: BindT -> (BindN, Entry)
bindTParam (BT l i t) = (BN l i, Def Val (BT l i t) ())

extendT = uncurry M.insert . bindTParam

fixType :: Env -> C.Expr -> C.Type -> C.Expr
fixType env (C.Constraint _ et e) t = fixType env e $ unify env t et
fixType env (C.E          l et e) t = tc env $ C.E l (unify env t et) e

tc :: Env -> C.Expr -> C.Expr
tc env (C.Constraint _ t e) = fixType env e t
tc env (C.E          l t e) = case e of

  -- Need too look up to get proper loc
  Num n -> se (unify env t $ C.T $ TIdent $ Bound Val Intrinsic "Number") $ Num n
  Str s -> se (unify env t $ C.T $ TIdent $ Bound Val Intrinsic "String") $ Str s

  Ident (Unbound    _)       -> se TUnknown e
  e'@(Ident (Bound _ il ii)) ->  k e'
    where k = case M.lookup (BN il ii) env of
            Nothing                    -> error "internal: identifier was reported bound"
            Just (Def _ (BT _ _ t') _) -> se $ unify env t t'
            Just (Data  _           _) -> se $ unify env t $ T TType

  Let (Def kind b@(BT vl vi vt) v) e -> C.E l t' $ Let newDef e'
    where v'@(C.E _ vt' _) = fixType env v vt
          newDef           = Def kind (BT vl vi vt') v'
          env'             = extendT b env
          e'@(C.E _ t'  _) = fixType env' e t

  Let (Data i@(BN _ di) variants) e -> se t' $ Let data' e'
    where fixVariant :: Variant BindT BindN -> Variant BindT BindN
          fixVariant (Variant vi args) = Variant vi $ (fmap . fmap) fixField args
            where fixField :: BindT -> BindT
                  fixField (BT bl bi t) = BT bl bi $ checkT env1 t

          bindConstrs :: Variant C.BindT BindN -> (BindN , Entry)
          bindConstrs (Variant b@(BN vl vi) ps) =
            (b, Def Val (BT vl vi $ T $ k $ TIdent $ Bound Val l di) ())
            where k = case ps of
                    Nothing     -> id
                    Just params -> TFun (for params $ \(BT _ _ t) -> t) . T

          envData         :: Entry -- phantom ex type parameter nessesitates rebuild
          envData          = Data i variants
          -- | env + type, without constructors
          env1             = M.insert i envData env
          env2             = extendMap env1 $ M.fromList $ (i, envData) : fmap bindConstrs variants'
          variants'        = fixVariant <$> variants
          e'@(C.E _ t'  _) = fixType env2 e t
          data'            = Data i variants'

  Assign i v -> se t'' $ Assign i v' -- immutability errors caught downstream in Pyrec.Report
    where t' = case i of
            Unbound _     -> t
            Bound _ al ai -> case M.lookup (BN al ai) env of
              Nothing                    -> error "internal: identifier was reported bound"
              Just (Def _ (BT _ _ t') _) -> t'
              Just (Data  _           _) -> T TType

          v'@(C.E _ t'' _) = fixType env v t'

  Fun params body -> se t' $ Fun params' body'
    where params'              = for params $ \(BT l i t) -> BT l i $ checkT env t
          env'                 = extendMap env $ M.fromList $ bindTParam <$> params'
          body'@(C.E _ retT _) = tc env' body
          t'                   = unify env t $ C.T
                                    $ TFun (for params' $ \(BT _ _ pt) -> pt) retT

  FunT params body -> se t' $ FunT params body'
    where env'                 = extendMap env $ M.fromList $ bindNParam <$> params
          body'@(C.E _ retT _) = tc env' body
          t'                   = unify env t $ C.T $ TParam params retT

  App f args -> se t' $ App f' args'
    where args'                = tc env <$> args
          ft                   = C.T $ flip TFun t $ for args' $ \(C.E _ t _) -> t
          f'@(C.E _ ft' _)     = fixType env f ft
          t'                   = case ft' of
            C.T (TFun _ retT) -> retT
            _                 -> C.TError $ C.TypeMismatch ft ft'

  AppT f args -> se t' $ AppT f' args
    where (C.E fl ft fe)       = tc env f
          expected             = C.T $ flip TParam t $ for [1..length args]
                                 $ \n -> BN fl $ 'T' : show n
          (t', ft')            = case ft of
            C.T (TParam params retT) ->
              (,ft) $ case map2S (,) params (checkT env <$> args) of
                Just substs -> unify env t $ subst (M.fromList substs) retT
                Nothing     -> TError $ TypeMismatch t retT
            _                        -> (ft, TError $ TypeMismatch expected ft)
          f'                   = C.E fl ft' fe

  Cases vt v cases -> se t'' $ Cases vt' v' cases'
    where v'@(C.E _ vt' _) = fixType env v vt

          bind :: C.Type -> Pattern -> (Bool , Pattern , Env)
          bind t p = case p of

            (Binding b@(BT l i t')) -> (True, Binding $ b', extendT b' M.empty )
              where t'' = unify env t t'
                    b'  = BT l i t''

            (Constr ci pats) -> result
              where ensureIdent (T (TIdent (Bound Val l i))) = Just $ BN l i
                    ensureIdent _                            = Nothing

                    -- outer maybe signifies error, inner maybe is use to distinguish
                    -- pyret's "| Constructor()" vs "| Constructor"
                    params :: Maybe (Maybe [C.Type])
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
                      Nothing     -> (False , (fmap . fmap) (bind TUnknown) pats)
                      Just params -> (True  , zipWith bind <$> params <*> pats)

                    result = case results of
                      Nothing   -> ( newerr
                                   , Constr ci Nothing
                                   , M.empty)
                      Just list -> ( and $ newerr : errs
                                   , Constr ci $ Just pats
                                   , M.unions envs)
                        where (errs, pats, envs) = unzip3 list

          checkCase :: (Bool, C.Type)  -> Case C.BindT BindN C.Expr ->
                       ((Bool , C.Type) , Case C.BindT BindN C.Expr)
          checkCase (err, t) (Case pat body) = ((err && err' , t') , Case pat' body')
            where (err' , pat' , caseEnv) = bind vt' pat
                  body'@(C.E _ t'  _)     = fixType caseEnv body t

          ((err , t') , cases') = mapAccumL checkCase (True, t) cases

          t'' = if err then TError $ C.CantCaseAnalyze t' else t'

  EmptyObject -> se (unify env t $ C.T $ TObject M.empty) EmptyObject

  -- can this be made less ridiculous?
  Extend obj fi fv -> se t' $ Extend obj' fi fv'
    where fv' @(C.E _ ft  _) = tc env fv

          ot                 = case t of
            C.T (TObject o)   -> C.T $ TObject $ trim o
            C.PartialObj p    -> C.PartialObj  $ trim p
            _                 -> C.PartialObj  $ M.empty
          trim map           = M.delete fi map

          obj'@(C.E _ ot' _) = fixType env obj ot

          t'                 = unify env t $ case ot' of
            C.T (TObject o)   -> C.T $ TObject $ tryRefine o
            C.PartialObj p    -> C.PartialObj  $ tryRefine p
            _                 -> C.PartialObj  $ M.singleton fi ft
          tryRefine map      = M.insert fi ft map

  Access obj fi -> se t'' $ Access obj' fi
    where t'                 = C.PartialObj $ M.singleton fi t
          obj'@(C.E _ ot  _) = fixType env obj t'
          t''                = unify env t $ case ot of
            C.T (TObject o)   -> tryRefine o
            C.PartialObj p    -> tryRefine p
            _                 -> t'
          tryRefine map      = fromMaybe t' $ M.lookup fi map

  where se = C.E l


stripBN (BN _ n) = n

subst :: Map BindN C.Type -> C.Type -> C.Type
subst substs = \case
  t@(C.T (TIdent (Bound Val l i))) -> fromMaybe t $ M.lookup (BN l i) substs
  t@(C.T (TIdent (Unbound _)))     -> t
  C.T (TParam params retT)         -> C.T $ TParam params $ recur retT
  C.T inner                        -> C.T        $ recur <$> inner
  PartialObj fields                -> PartialObj $ recur <$> fields
  t@(TError _)                     -> t
  t@(TUnknown)                     -> t -- redundancy to future-proof
  where recur = subst substs

checkT :: Env -> C.Type -> C.Type
checkT env = \case
  t@(C.T (TIdent (Bound Val l i))) -> case M.lookup (BN l i) env of
    Just (Def Val (BT _ _ (C.T TType)) _) -> t
    _                                     -> error "unbound, or non-type identifier"
  t@(C.T (TIdent (Unbound _)))     -> t
  C.T (TParam params retT)         ->
    flip checkT retT $ extendMap env $ M.fromList $ bindNParam <$> params
  C.T inner                        -> C.T        $ checkT env <$> inner
  PartialObj fields                -> PartialObj $ checkT env <$> fields
  t@(TError _)                     -> t
  t@(TUnknown)                     -> t -- redundancy to future-proof

unify :: Env -> C.Type -> C.Type -> C.Type
unify env t1 t2 = unifyWithSubsts env M.empty t1 t2

-- expected then got
unifyWithSubsts :: Env -> Map BindN BindN -> C.Type -> C.Type -> C.Type
unifyWithSubsts env substs _a _b = case (_a, _b) of
  (C.T TType, C.T TType) -> C.T TType

  (C.T (TIdent i@(Bound Val al ai)), C.T (TIdent (Bound Val bl bi))) ->
    try $ C.T <$> do
      -- either a substitutes for b, or a has no substitution and a == b
      guard $ case M.lookup a substs of
        Just a' -> a' == b
        Nothing -> a  == b
      return $ TIdent i
    where a = (BN al ai)
          b = (BN bl bi)

  (C.T (TFun aParams aRes), C.T (TFun bParams bRes)) ->
    try $ C.T <$> do
      params <- map2S recur aParams bParams
      return $ TFun params $ recur aRes bRes

  (C.T (TParam aParams aRes), C.T (TParam bParams bRes)) ->
    try $ C.T <$> do
      news <- map2S (,) aParams bParams
      let substs' = extendMap substs $ M.fromList news
      return $ TParam aParams $ unifyWithSubsts env substs' aRes bRes

  ((C.T (TObject o1)), (C.T (TObject o2))) ->
    try $ C.T <$> TObject <$> do
      guard $ M.size o1 == M.size o2             -- same size
      let match = M.intersectionWith recur o1 o2 -- unify common elements
      guard $ M.size match == M.size o1          -- intersection same size means ident keys
      return match

  (l@(C.T (TObject _)), r@(C.PartialObj _)) ->
    recur r l                                    -- flip args and recur
  (C.PartialObj p, C.T (TObject o)) ->
    try $ C.T <$> TObject <$> do
      guard $ M.isSubmapOfBy (\_ _ -> True) p o  -- is partial subset
      let update = M.intersectionWith recur o p  -- unify fields in common
      return $ M.union update o                  -- perform updates

  (C.PartialObj p1, C.PartialObj p2) -> C.PartialObj $ p1 `M.union` p2

  (C.TUnknown, other)      -> checkT env other
  (other,      C.TUnknown) -> checkT env other

  (TError e1,  TError e2)  -> TError $ unifyError env substs e1 e2
  (a,          b)          -> TError $ TypeMismatch a b

  where recur = unifyWithSubsts env substs
        try :: Maybe C.Type -> C.Type
        try t = case t of
          Just t  -> t
          Nothing -> TError $ TypeMismatch _a _b

unifyError :: Env -> Map BindN BindN -> TypeError -> TypeError -> TypeError
unifyError env substs _a _b = case (_a, _b) of
  (TypeMismatch a1 a2, TypeMismatch b1 b2) -> TypeMismatch (mutRecur a1 b1) $ mutRecur a2 b2
  (CantCaseAnalyze a,  CantCaseAnalyze b)  -> CantCaseAnalyze $ mutRecur a b
  _                                        -> error "I DON'T EVEN"
  where mutRecur = unifyWithSubsts env substs
