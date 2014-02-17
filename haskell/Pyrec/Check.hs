module Pyrec.Check where

import qualified Data.Map as M
import           Data.Map (Map)

import           Pyrec.AST hiding ()
import           Pyrec.AST.Parse as P
import qualified Pyrec.AST.Check as C

type Entry = Decl P.Bind P.Id ()

type Env = Map P.Id Entry

fixType :: Env -> P.Expr -> P.Type -> C.Expr
fixType env (P.E l et e) t = tc env $ P.E l (unify et t env) e


tc :: Env -> P.Expr -> C.Expr
tc env (P.E l t e) = case e of
  
  Num n -> se (unify t (P.T TNum) env) $ Num n
  Str s -> se (unify t (P.T TStr) env) $ Str s
  
  Ident i -> case M.lookup i env of
    Nothing -> C.Error (C.Unbound i) $ se t $ Ident $ C.Bound C.bad i
    Just (Def _ (P.B l _ t') _) -> e'
      where i' = C.Bound l i
            e' = se (unify t t' env) $ Ident i'
  
  Let (Def kind b@(P.B vl vi vt) v) e -> C.E l t' $ Let (newDef v') $ e'
    where v'@(C.E _ vt' _) = fixType env v vt
          newDef q         = Def kind (P.B vl vi vt') q
          env'             = M.insert vi $ newDef ()
          e'@(C.E _ t'  _) = fixType env e t
  
  Assign i v -> case M.lookup i env of
    Nothing -> C.Error (C.Unbound i) $ se t $ Ident $ C.Bound C.bad i
    Just (Def dt (P.B l _ t') _) -> f $ se t'' $ Assign i' v'
      where i' = C.Bound l i
            v'@(C.E _ t'' _) = fixType env v $ unify t t' env
            f  = case dt of
              Val -> C.Error (C.MutateVal i')
              Var -> id
  
  App f args -> k $ App f' args'
    where f'@(C.E _ ft' _) = fixType env f ft
          args' = map (tc env) args
          getT (C.E _ t _) = t
          ft = P.T $ TFun (map getT args') t
          k = case ft' of
            P.T (TFun _ retT) -> se retT
            _                 -> C.Error (C.TypeError ft ft') . se t
  
  where se = C.E l


-- prob don't need
checkT :: P.Type -> Env -> P.Type
checkT t env = unify TUnknown t env

unify :: P.Type -> P.Type -> Env -> P.Type
unify = undefined
