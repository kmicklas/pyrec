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

data Store ex = Store Int (IntMap ex) -- int for bump pointer malloc

type Eval id ex m = Env id -> ex -> m ex

emptyHeap :: MonadState (Store ex) m => m ()
emptyHeap = put $ Store 0 IM.empty

load :: MonadState (Store ex) m => Int -> m ex
load i = do (Store _ m) <- get
            return $ m IM.! i

-- ASSUMES e IS EVALUATEED ALREADY!
storeNew :: MonadState (Store ex) m => ex -> m Int
storeNew e = do (Store i _) <- get
                let i' = i + 1
                store i' e
                (Store _ m') <- get
                put $ Store i' m'
                return i'

store :: MonadState (Store ex) m => Int -> ex -> m ()
store i e = do (Store i2 m) <- get
               put $ Store i2 $ IM.insert i e m


bind :: (MonadState (Store ex) m, Ord id) => Eval id ex m ->
        (bd -> id) -> Env id -> bd -> ex -> ex -> Bool -> m ex
bind eval' bdToId env binding be reste isMut = do
    bv  <- eval' env be
    ind <- storeNew bv
    eval' (M.insert (bdToId binding) (ind, isMut) env) reste


eval :: (MonadState (Store ex) m, Ord id) =>
        Eval id ex m -> (Expr bd id ex ty -> ex) -> (String -> id) ->
        (bd -> id) -> Env id -> Expr bd id ex ty -> m ex
eval eval' mkE mkID bdToId env e = case e of

  (Def (Let (Val bd be)) reste) -> bind eval' bdToId env bd be reste False
  (Def (Let (Var bd be)) reste) -> bind eval' bdToId env bd be reste True

  (Assign iden expr) -> let
    (ptr, isMut) = env M.! iden
    in if isMut
       then error "Cannot mutate non-variable"
       else do bv <- eval' env expr
               store ptr bv
               eval' env $ mkE $ Ident $ mkID "__Nothing"
