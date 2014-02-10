module Check where

import Data.Map as M
import Data.Map (Map)

type Loc = Int
type Id = (String, Loc)

data LExpr = At Loc Expr Expr deriving Eq
data TExpr = As Expr Expr deriving Eq

type Expr = PExpr LExpr

data PExpr e
  = Let (Bind e) e
  | Graph [Bind e] e
  | Assign Id e
  | Cases e [Case e]
  | Try e e
  | Fun [Id] e
  | App e [e]
  | Id Id
  | Number Double
  | Str String
  | Error String e
  | TType
  | TUnknown
  | TAny
  | TNum
  | TStr
  | TFun [Id] [Arg e] e
  deriving Eq

data Bind e
  = Val Id e e
  | Var Id e e
  | Data Id [Id] [Variant e]
  deriving Eq

data Arg e = Arg Id e deriving Eq

data Case e = Case Pattern e deriving Eq

data Pattern
  = Constr Id (Maybe [Pattern])
  | Bind Id
  deriving Eq

data Variant e = Variant Id (Maybe [Arg e]) deriving Eq





type Env = Map Id LExpr

eval :: LExpr -> Env -> LExpr
eval (At l et e) env = At l et $ evalE e env

evalE :: Expr -> Env -> Expr
evalE (Let (Val n v) e) = eval e $ insert n (eval v env) env
evalE (Let (Var n v) e) = Let (Var n (eval v env)) $ e
evalE e = e

check :: LExpr -> Expr -> Env -> LExpr
check (At l et e) t env = At l t' e'
  where (As t' e') = tc e (unify t et env) env


tc :: Expr -> Expr -> Env -> TExpr

tc (Let (Val n v) e) t env = As t' (Let (Val n v') e')
  where v'@(At _ vt _) = check v TUnknown env
        e'@(At _ t' _) = check e t $ M.insert n vt env

tc (Let (Var n v) e) t env = As t' (Let (Var n v') e')
  where v'@(At _ vt _) = check v TUnknown env
        e'@(At _ t' _) = check e t $ M.insert n vt env

tc (Assign n v) t env = As t' v'
  where (At _ t' v') = check v t env

tc (App f as) t env = undefined

tc e@(Id n) t env = case M.lookup n env of
                      Just (At _ nt _) -> As (unify t nt env) e
                      Nothing -> As t e

unify :: Expr -> Expr -> Env -> Expr
unify TUnknown t _ = t
unify t TUnknown _ = t
unify a b _ | a == b = a
unify _ _ _ = error "no unify"
