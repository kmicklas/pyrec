{-# LANGUAGE FlexibleContexts #-}
module Pyrec.Interp where

import Control.Monad
import Control.Monad.State

import Data.Functor.Identity

import qualified Data.Map as M
import Data.Map (Map)

import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

import Data.Maybe

import Pyrec.AST


type Id = String

newtype Expr = E (PExpr Id Expr)

type Env = Map Id (Int, Bool)

data Store = Store Int (IntMap Expr) -- int for bump pointer malloc


emptyHeap :: MonadState Store m => m ()
emptyHeap = put $ Store 0 IM.empty

load :: MonadState Store m => Int -> m Expr
load i = do (Store _ m) <- get
            return $ m IM.! i

-- ASSUMES e IS EVALUATEED ALREADY!
storeNew :: MonadState Store m => Expr -> m Int
storeNew e = do (Store i _) <- get
                let i' = i + 1
                store i' e
                (Store _ m') <- get
                put $ Store i' m'
                return i'

store :: MonadState Store m => Int -> Expr -> m ()
store i e = do (Store i2 m) <- get
               put $ Store i2 $ IM.insert i e m


bind :: MonadState Store m => Env -> Id -> Expr -> Expr -> Bool -> m Expr
bind env iden be reste isMut = do
    bv  <- eval env be
    ind <- storeNew bv
    eval (M.insert iden (ind, isMut) env) reste

eval :: MonadState Store m => Env -> Expr -> m Expr
eval env (E e) = case e of
  (Let (Val (Bind iden be)) reste) -> bind env iden be reste False
  (Let (Var (Bind iden be)) reste) -> bind env iden be reste True

  (Assign iden expr) -> let
    (ptr, isMut) = env M.! iden
    in if isMut
       then error "Cannot mutate non-variable"
       else do bv <- eval env expr
               store ptr bv
               eval env $ E $ Ident "__Nothing"
