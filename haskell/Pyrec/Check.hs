module Pyrec.Check where

import Pyrec.AST

type Env = [Def LExpr]

tc :: LExpr -> Type -> Env -> LExpr
tc (At l et (Def (Let (Val (Bind id vt) v)) e)) t env =
  let ut             = unify et t env
      vc@(At _ vt _) = tc v (checkT vt env) env
      newDef         = Let (Val (Bind id vt) vc)
      newEnv         = newDef : env
  in At l ut $ Def newDef $ tc e ut newEnv

checkT :: Type -> Env -> Type
checkT t env = unify TUnknown t env

unify :: Type -> Type -> Env -> Type
unify = undefined
