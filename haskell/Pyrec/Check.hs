module Pyrec.Check where

import qualified Data.Map as M
import Data.Map (Map)
import Pyrec.AST

type Env = [Def LExpr]

lookup :: Loc -> Id -> Env -> LExpr
lookup l id [] = At l TUnknown $ Error ("Unbound identifier: " + show id)
                                       $ At l TUnkown $ Id id
lookup l id (Val (Bind n _) v) | id == n = v
lookup l id (Data n _ 

eval :: LExpr -> Env -> LExpr
eval (At _ _ (Def (Let (Val (Bind id _) v)) e)) env = eval e $ insert id (eval v env) env
eval e@(At l t (Ident id)) env =
  case M.lookup id env of
    Just v  -> v
    Nothing -> At l t $ Error (show id ++ " not bound") e
eval e@(At l t _) _ = At l t $ Error ("Don't know how to handle") e

check :: LExpr -> Expr -> Env -> LExpr
check (At l et e) t env = At l t' e'
  where (As t' e') = tc e (unify t et env) env

tc :: Expr -> Expr -> Env -> TExpr

tc (Let (Val n v) e) t env = As t' (Let (Val n v') e')
  where v'@(At _ vt _) = check v TUnknown env
        e'@(At _ t' _) = check e t $ M.insert n vt env

tc (Let (Var n _ v) e) t env = As t' (Let (Var n v') e')
  where v'@(At _ vt _) = check v TUnknown env
        e'@(At _ t' _) = check e t $ M.insert n vt env

tc (Assign n v) t env = As t' v'
  where (At _ t' v') = check v t env

tc (App f as) t env = undefined

tc e@(Ident n) t env = case M.lookup n env of
                         Just (At _ nt _) -> As (unify t nt env) e
                         Nothing -> As t e

unify :: Expr -> Expr -> Env -> Expr
unify TUnknown t _ = t
unify t TUnknown _ = t
unify a b _ | a == b = a
unify _ _ _ = error "no unify"
