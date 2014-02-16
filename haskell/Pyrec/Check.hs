module Pyrec.Check where

import Pyrec.AST

type Env = [Def LExpr]

fixType :: LExpr -> Type -> Env -> LExpr
fixType (At l et e) t env = tc (At l (unify et t env) e) env

tc :: LExpr -> Env -> LExpr
tc (At l t e) env = case e of
  (Num n) -> At l (unify t TNum env) $ Num n
  (Str s) -> At l (unify t TStr env) $ Str s
  (Ident id) -> At l (unify t t' env) $ Ident id'
    where (id', t') = resolve id env

  (Def (Let (Val (Bind id vt) v)) e) -> tcLet l t id vt v e env Val
  (Def (Let (Var (Bind id vt) v)) e) -> tcLet l t id vt v e env Var

  (Assign id v) -> At l t $ Assign id' $ fixType v t' env
    where (id', t') = resolve id env



tcLet l t id vt v e env kind = At l t' $ Def newDef $ e'
  where vc@(At _ vt' _) = fixType v vt env
        newDef          = Let (kind (Bind id vt') vc)
        newEnv          = newDef : env
        e'@(At _ t'  _) = fixType e t newEnv

resolve :: Id -> Env -> (Id, Type)
resolve = undefined

checkT :: Type -> Env -> Type
checkT t env = unify TUnknown t env

unify :: Type -> Type -> Env -> Type
unify = undefined
