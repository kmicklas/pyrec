module Pyrec.ScopeCheck where

import Control.Applicative

import qualified Data.Map as M
import           Data.Map (Map)

import           Pyrec.AST
import qualified Pyrec.AST.Parse as P
import qualified Pyrec.AST.Scope as S

type Env = Map P.ID S.ID

sc :: Env -> P.Expr -> S.Expr
sc env (P.E l t e) = S.E l t $ case e of

  (Def (Graph bds) ex) -> Def (Graph bds') $ sc env' ex
    where env' = foldl extendEnv env $ f <$> bds

          f (Val b _) = (b, False)
          f (Var b _) = (b, True)

          g h (Val b v) = Val b $ h v
          g h (Var b v) = Val b $ h v

          bds' :: [Decl P.Bind S.ID S.Expr]
          bds' = g (sc env') <$> bds

  (Def (Let d) ex) -> case d of
    (Val b v) -> helper Val b v False
    (Var b v) -> helper Var b v True
    (Data _ _) -> undefined -- TEMP HACK
    where helper kind  b v isMut =
            Def (Let (kind b (sce v))) $ sc env' ex
            where env' = extendEnv env (b, isMut)

  (Fun bds ex) -> Fun bds $ sc env' ex
    where env' = foldl extendEnv env $ (\b -> (b, False)) <$> bds

  (Ident i)    -> Ident $ lookup i
  (Assign i e) -> Assign (lookup i) $ sce e

  (App f args) -> App (sce f) $ sce <$> args
  (Try e1 b e2) -> Try (sce e1) b $ sce e2

  (Cases ty ex cs) -> undefined -- TEMP HACK --Cases ty (sce ex) cs

  (Num d) -> Num d
  (Str s) -> Str s

  where sce = sc env
        lookup i = case M.lookup i env of
          Just i' -> i'
          Nothing -> S.Free i

        extendEnv :: Env -> (P.Bind, Bool) -> Env
        extendEnv env (b@(P.B _ s _), isMut) =
          M.insert s (S.Bound b isMut) env

{-
sct (P.T t) = S.T $ case t of
  (TIdent id)   -> S.Free id -- TEMP HACK
  (TFun ps ret) -> TFun (sct <$> ps) $ sct ret
-}
