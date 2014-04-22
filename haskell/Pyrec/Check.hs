module Pyrec.Check where

import           Prelude                 hiding (map, mapM)

import           Control.Applicative
import           Control.Monad           hiding (mapM)
import           Control.Monad.Reader    hiding (mapM)

import           Data.List                      (find, mapAccumL)
import qualified Data.Map          as M
import           Data.Map                       (Map)
import           Data.Traversable        hiding (for, mapAccumL)

import           Pyrec.Desugar                  (Entry)

import           Pyrec.Misc

import qualified Pyrec.IR          as IR        (Pattern)
import           Pyrec.IR                hiding (Pattern)
import           Pyrec.IR.Desugar  as D
import qualified Pyrec.IR.Check    as C

type Pattern = IR.Pattern D.BindT BindN
type Env     = Map Id Entry
type CK      = Reader Env

emptyEnv :: Env
emptyEnv = M.empty

fixType :: Env -> D.Expr -> D.Type -> C.Expr
fixType env (D.E l et e) t = tc env $ D.E l (unify env et t) e

tc :: Env -> D.Expr -> C.Expr
tc env (D.E l t e) = case e of

  Num n -> se (unify env t (D.T TNum)) $ Num n
  Str s -> se (unify env t (D.T TStr)) $ Str s

  Ident i -> case M.lookup i env of
    Nothing -> se t $ Ident $ C.Unbound i
    Just (Def dt (BT l _ t') _) -> e'
      where i' = C.Bound dt l i
            e' = se (unify env t t') $ Ident i'

  Let (Def kind b@(BT vl vi vt) v) e -> C.E l t' $ Let (newDef v') e'
    where v'@(C.E _ vt' _) = fixType env v vt
          newDef q         = Def kind (BT vl vi vt') q
          env'             = M.insert vi (newDef ()) env
          e'@(C.E _ t'  _) = fixType env' e t

  Let (Data i@(BN _ bi) variants) e -> se t' $ Let data' e'
    where envData         :: Entry -- phantom ex type parameter nessesitates rebuild
          envData          = Data i variants

          fixVar :: Variant D.BindT BindN -> Variant D.BindT BindN
          fixVar (Variant vi args) = Variant vi $ fmap fmap fmap fixField args
            where fixField :: D.BindT -> D.BindT
                  fixField (BT bl bi t) = BT bl bi $ checkT (M.insert bi envData env) t

          variants'        = fixVar <$> variants

          bindConstrs :: Variant D.BindT BindN -> (Id , Entry)
          bindConstrs (Variant (BN vl vi) ps) = (vi, Def Val (BT vl vi $ T $ k $ TIdent vi) ())
            where k = case ps of
                    Nothing     -> id
                    Just params -> TFun (for params $ \(BT _ _ t) -> t) . T

          env'             = M.union (M.fromList $ (bi, envData) : fmap bindConstrs variants') env
          e'@(C.E _ t'  _) = fixType env' e t

          data'            = Data i variants'

  Assign i v -> case M.lookup i env of
    Nothing -> se t $ Ident $ C.Unbound i
    Just (Def dt (BT l _ t') _) -> se t'' $ Assign (C.Bound dt l i) v'
      where v'@(C.E _ t'' _) = fixType env v $ unify env t t'

  App f args -> se t' $ App f' args'
    where f'@(C.E _ ft' _) = fixType env f ft
          args' = tc env <$> args
          getT (C.E _ t _) = t
          ft = D.T $ TFun (getT <$> args') t
          t' = case ft' of
            D.T (TFun _ retT) -> retT
            _                 -> D.TError $ D.TypeMismatch ft ft'

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
                    params = do (Variant vi params) <-
                                  ensureIdent t
                                  >>= (\i -> M.lookup i env)
                                  >>= (\x -> case x of
                                          Data _ vs -> Just vs
                                          _         -> Nothing)

                                  >>= find (\(Variant vi _) -> vi == ci)
                                guard $ (length <$> params) == (length <$> pats)
                                return $ flip (fmap fmap fmap) params $ \(BT _ _ t) -> t

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

  where se = C.E l


checkT :: Env -> D.Type -> D.Type
checkT env TUnknown     = TUnknown
checkT env t@(D.T TNum) = t
checkT env t@(D.T TStr) = t
checkT env (D.T (TFun args res)) = D.T $ TFun (checkT env <$> args) res

-- expected then got
unify :: Env -> D.Type -> D.Type -> D.Type
unify env D.TUnknown t          = checkT env t
unify env t D.TUnknown          = checkT env t
unify env (D.T TNum) (D.T TNum) = D.T TNum
unify env (D.T TStr) (D.T TStr) = D.T TStr
unify env a@(D.T (TFun aArgs aRes)) b@(D.T (TFun bArgs bRes)) =
  if length aArgs == length bArgs
  then D.T $ TFun (zipWith (unify env) aArgs bArgs) $ unify env aRes bRes
  else TError $ TypeMismatch a b
unify env a@(D.T _) b@(D.T _) = TError $ TypeMismatch a b
