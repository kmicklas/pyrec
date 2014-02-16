module Pyrec.Check where

import Pyrec.AST

type Env = [Def LExpr]

fixType :: LExpr -> Type -> Env -> LExpr
fixType (At l et e) t env = tc (At l (unify et t env) e) env

tc :: LExpr -> Env -> LExpr

tc (At l t (Def (Let (Val (Bind id vt) v)) e)) env =
  tcLet l t id vt v e env Val

tc (At l t (Def (Let (Var (Bind id vt) v)) e)) env =
  tcLet l t id vt v e env Var

tc (At l t (Assign id v)) env = At l t $ Assign id' $ fixType v t' env
  where (id', t') = resolve id env

tc (At l t (Ident id)) env = At l (unify t t' env) $ Ident id'
  where (id', t') = resolve id env

tc (At l t (Number n)) env = At l (unify t TNum env) $ Number n

tcLet l t id vt v e env kind =
  let vc@(At _ vt _) = fixType v (checkT vt env) env
      newDef         = Let (kind (Bind id vt) vc)
      newEnv         = newDef : env
  in At l t $ Def newDef $ fixType e t newEnv

resolve :: Id -> Env -> (Id, Type)
resolve = undefined

checkT :: Type -> Env -> Type
checkT t env = unify TUnknown t env

unify :: Type -> Type -> Env -> Type
unify = undefined
