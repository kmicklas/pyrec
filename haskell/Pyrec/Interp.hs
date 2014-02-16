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

type Env id = Map id (Int, Bool)

data Store e = Store Int (IntMap e) -- int for bump pointer malloc

type Eval id e m = Env id -> e -> m e

emptyHeap :: MonadState (Store e) m => m ()
emptyHeap = put $ Store 0 IM.empty

load :: MonadState (Store e) m => Int -> m e
load i = do (Store _ m) <- get
            return $ m IM.! i

-- ASSUMES e IS EVALUATEED ALREADY!
storeNew :: MonadState (Store e) m => e -> m Int
storeNew e = do (Store i _) <- get
                let i' = i + 1
                store i' e
                (Store _ m') <- get
                put $ Store i' m'
                return i'

store :: MonadState (Store e) m => Int -> e -> m ()
store i e = do (Store i2 m) <- get
               put $ Store i2 $ IM.insert i e m


bind :: (MonadState (Store e) m, Ord id) => Eval id e m ->
        Env id -> id -> e -> e -> Bool -> m e
bind eval' env iden be reste isMut = do
    bv  <- eval' env be
    ind <- storeNew bv
    eval' (M.insert iden (ind, isMut) env) reste

eval :: (MonadState (Store e) m, Ord id) =>
        Eval id e m -> (PExpr id e -> e) -> (String -> id) ->
        Env id -> PExpr id e -> m e
eval eval' mkE mkID env e = case e of
  (Let (Val (Bind iden be)) reste) -> bind eval' env iden be reste False
  (Let (Var (Bind iden be)) reste) -> bind eval' env iden be reste True

  (Assign iden expr) -> let
    (ptr, isMut) = env M.! iden
    in if isMut
       then error "Cannot mutate non-variable"
       else do bv <- eval' env expr
               store ptr bv
               eval' env $ mkE $ Ident $ mkID "__Nothing"
