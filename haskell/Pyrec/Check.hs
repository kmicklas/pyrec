module Pyrec.Check where

import           Control.Monad
import           Control.Applicative

import           Data.List (find, mapAccumL)

import qualified Data.Map as M
import           Data.Map (Map)

import           Pyrec.Misc

import           Pyrec.AST
import           Pyrec.AST.Parse as P
import qualified Pyrec.AST.Check as C

type Entry = Decl P.Bind P.Id ()

type Env = Map P.Id Entry

type PatternP = Pattern P.Bind P.Id
type PatternC = Pattern P.Bind C.Id

emptyEnv :: Env
emptyEnv = M.empty

fixType :: Env -> P.Expr -> P.Type -> C.Expr
fixType env (P.E l et e) t = tc env $ P.E l (unify env et t) e


tc :: Env -> P.Expr -> C.Expr
tc env (P.E l t e) = case e of

  Num n -> se (unify env t (P.T TNum)) $ Num n
  Str s -> se (unify env t (P.T TStr)) $ Str s

  Ident i -> case M.lookup i env of
    Nothing -> se t $ Ident $ C.Unbound i
    Just (Def _ (P.B l _ t') _) -> e'
      where i' = C.Bound l i
            e' = se (unify env t t') $ Ident i'

  Let (Def kind b@(P.B vl vi vt) v) e -> C.E l t' $ Let (newDef v') $ e'
    where v'@(C.E _ vt' _) = fixType env v vt
          newDef q         = Def kind (P.B vl vi vt') q
          env'             = M.insert vi (newDef ()) env
          e'@(C.E _ t'  _) = fixType env' e t

  Let (Data i variants) e -> se t' $ Let data' e'
    where envData         :: Entry -- phantom ex type parameter nessesitates rebuild
          envData          = Data i variants

          fixVar :: Variant P.Bind P.Id -> Variant P.Bind P.Id
          fixVar (Variant vi args) = Variant vi $ fmap fmap fmap fixField args
            where fixField :: P.Bind -> P.Bind
                  fixField (B bl bi t) = B bl bi $ checkT (M.insert i envData env) t

          variants'        = map fixVar variants

          bindConstrs :: Variant P.Bind P.Id -> (P.Id , Entry)
          bindConstrs (Variant vi ps) = (,) vi $ Def Val (B l vi $ T $ k $ TIdent vi) ()
          {- not ideal l :: Loc -}
            where k = case ps of
                    Nothing     -> id
                    Just params -> TFun (map (\(B _ _ t) -> t) params) . T

          env'             = M.union (M.fromList $ (i, envData) : map bindConstrs variants') env
          e'@(C.E _ t'  _) = fixType env' e t

          variants''       = for variants' $ \(Variant vi args) -> Variant (C.Bound l vi) args
          {- not ideal l :: Loc -}
          data'            = Data (C.Bound l i) variants''

  Assign i v -> case M.lookup i env of
    Nothing -> se t $ Ident $ C.Unbound i
    Just (Def dt (P.B l _ t') _) -> se t'' $ Assign i' v'
      where v'@(C.E _ t'' _) = fixType env v $ unify env t t'
            i' = case dt of
              Val -> C.NotMutable l i
              Var -> C.Bound l i

  App f args -> se t' $ App f' args'
    where f'@(C.E _ ft' _) = fixType env f ft
          args' = map (tc env) args
          getT (C.E _ t _) = t
          ft = P.T $ TFun (map getT args') t
          t' = case ft' of
            P.T (TFun _ retT) -> retT
            _                 -> (P.TError $ P.TypeMismatch ft ft')

  Cases vt v cases -> se t'' $ Cases vt' v' cases'
    where v'@(C.E _ vt' _) = fixType env v vt

          bind :: P.Type -> PatternP -> (Bool , PatternC , Env)
          bind t p = case p of

            (Binding (B l i t')) -> ( True
                                    , Binding $ B l i t''
                                    , M.singleton i $ Def Val (B l i t'') ())
              where t'' = unify env t t'

            (Constr ci pats) -> result
              where ensureIdent (T (TIdent i)) = Just i
                    ensureIdent _              = Nothing

                    -- outer maybe signifies error, inner maybe is use to distinguish
                    -- pyret's "| Constructor()" vs "| Constructor"
                    params :: Maybe (Maybe [P.Type])
                    params = do (Variant vi params) <-
                                  ensureIdent t
                                  >>= (\i -> M.lookup i env)
                                  >>= (\x -> case x of
                                          Data _ vs -> Just vs
                                          _         -> Nothing)

                                  >>= find (\(Variant vi _) -> vi == ci)
                                guard $ (length <$> params) == (length <$> pats)
                                return $ flip (fmap fmap fmap) params $ \(B _ _ t) -> t

                    results :: Maybe [(Bool, PatternC , Env)]
                    (newerr, results) = case params of
                      Nothing     -> (False , fmap fmap fmap (bind $ T $ TAny) pats )
                      Just params -> (True  , zipWith bind <$> params <*> pats      )

                    result = case results of
                      Nothing   -> ( newerr
                                   , Constr (C.Bound l ci) $ Nothing
                                   , M.empty)
                      Just list -> ( and $ newerr : errs
                                   , Constr (C.Bound l ci) $ Just pats
                                   , M.unions $ envs)
                        where (errs, pats, envs) = unzip3 list

          checkCase :: (Bool, P.Type) -> Case P.Bind P.Id P.Expr -> ((Bool , P.Type) , Case P.Bind C.Id C.Expr)
          checkCase (err, t) (Case pat body) = ((err && err' , t') , Case pat' body')
            where (err' , pat' , caseEnv) = bind vt' pat
                  body'@(C.E _ t'  _)     = fixType caseEnv body t

          ((err , t') , cases') = mapAccumL checkCase (True, t) cases

          t'' = if err then TError $ P.CantCaseAnalyze t' else t'

  where se = C.E l


checkT :: Env -> P.Type -> P.Type
checkT env TUnknown     = TUnknown
checkT env t@(P.T TNum) = t
checkT env t@(P.T TStr) = t
checkT env (P.T (TFun args res)) = P.T $ TFun (map (checkT env) args) res

-- expected then got
unify :: Env -> P.Type -> P.Type -> P.Type
unify env P.TUnknown t          = checkT env t
unify env t P.TUnknown          = checkT env t
unify env (P.T TNum) (P.T TNum) = P.T TNum
unify env (P.T TStr) (P.T TStr) = P.T TStr
unify env a@(P.T (TFun aArgs aRes)) b@(P.T (TFun bArgs bRes)) =
  if length aArgs == length bArgs
  then P.T $ TFun (zipWith (unify env) aArgs bArgs) $ unify env aRes bRes
  else TError $ TypeMismatch a b
unify env a@(P.T _) b@(P.T _) = TError $ TypeMismatch a b
