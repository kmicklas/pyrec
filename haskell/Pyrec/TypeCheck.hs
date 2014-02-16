module Pyrec.Check where

import Control.Applicative


import           Pyrec.AST hiding ()
-- import           Pyrec.AST.Parse
import qualified Pyrec.AST.Parse as S (Loc, Bind(B), Type(T))
import qualified Pyrec.AST.Scope as S
import           Pyrec.ScopeCheck (Env)


fixType :: S.Expr -> S.Type -> Env -> S.Expr
fixType (S.E l et e) t env = tc (S.E l (unify et t env) e) env


tc :: S.Expr -> Env -> S.Expr
tc (S.E l t e) env = case e of

  (Num n) -> S.E l (unify t (S.T TNum) env) $ Num n
  (Str s) -> S.E l (unify t (S.T TStr) env) $ Str s

  old@(Ident i) -> case i of
    (S.Bound (S.B _ _ t') _) -> S.E l (unify t t' env) $ Ident i
    (S.Free _)               -> S.E l t old -- Unbound Ident

  (Def (Let (Val (S.B vl vi vt) v)) e) -> tcLet l t vl vi vt v e env Val
  (Def (Let (Var (S.B vl vi vt) v)) e) -> tcLet l t vl vi vt v e env Var

  old@(Assign i v) -> case i of
    (S.Bound (S.B _ _ t') True) -> S.E l t'' $ Assign i $ e'
      where e'@(S.E _ t'' _) = fixType v (unify t t' env) env

    _ -> S.E l t old -- Unbound Ident, or mutation of non-variablex



tcLet l t vl vi vt v e env kind = S.E l t' $ Def newDef $ e'
  where v'@(S.E _ vt' _) = fixType v vt env
        newDef           = Let (kind (S.B vl vi vt') v')
--        newEnv           = newDef : env
        e'@(S.E _ t'  _) = fixType e t env

-- prob don't need
checkT :: S.Type -> Env -> S.Type
checkT t env = unify (S.T TUnknown) t env

unify :: S.Type -> S.Type -> Env -> S.Type
unify = undefined
