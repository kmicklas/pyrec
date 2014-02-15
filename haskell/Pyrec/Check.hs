module Pyrec.Check where

import Data.Map as M
import Data.Map (Map)

type Loc = Int

data Id
  = Name Loc String
  | Gen String
  deriving (Eq, Show, Ord)

data LExpr = At Loc Expr Expr deriving Eq
data TExpr = As Expr Expr deriving Eq

type Expr = PExpr LExpr

data PExpr e
  = Def (Def e) e
  | Assign Id e
  | Cases e [Case e]
  | Try e e
  | Fun [Id] e
  | App e [e]
  | ParamApp e [e]
  | Ident Id
  | Number Double
  | Str String
  | Error String e
  | TType
  | TParamType [Id]
  | TUnknown
  | TAny
  | TNum
  | TStr
  | TFun [Id] [Bind e] e
  | TParam [Id] e
  deriving Eq

data Def e
  = Let (Decl e)
  | Graph [Decl e]

data Decl e
  = Val (Bind e) e
  | Var (Bind e) e
  | Data Id [Id] [Variant e]
  deriving Eq

data Bind e = Bind Id e deriving Eq

data Case e = Case (Pattern e) e deriving Eq

data Pattern e
  = Constr Id (Maybe [Pattern e])
  | Binding (Bind e)
  deriving Eq

data Variant e = Variant Id (Maybe [Bind e]) deriving Eq





type Env = [Def LExpr]

lookup :: Loc -> Id -> Env -> LExpr
lookup l id [] = At l TUnknown $ Error ("Unbound identifier: " + show id)
                                       $ At l TUnkown $ Id id
lookup l id (Val (Bind n _) v) | id == n = v
lookup l id (Data n _ 

eval :: LExpr -> Env -> LExpr
eval (At _ _ (Let (Val (Bind id _) v) e)) env = eval e $ insert id (eval v env) env
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
