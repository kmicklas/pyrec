{-# LANGUAGE FlexibleContexts #-}
module Pyrec.Interp where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import qualified Data.Map as M
import Data.Map (Map)

import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

import Data.Maybe

import Pyrec.IR

type Env id = Map id (Int, DefType)

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

eval :: (MonadState (Store ex) m, Functor m, Ord id) =>
        Eval id ex m -> (Expr bt bn id ty ex -> ex) -> (ex -> Expr bt bn id ty ex) ->
        (String -> id) -> (bt -> id) -> (id -> bt) ->
        Env id -> Expr bt bn id ty ex -> m ex
eval eval' e2ex ex2e mkID b2i i2b env e = case e of

  Num _   -> return $ e2ex e
  Str _   -> return $ e2ex e
  Fun _ _ -> return $ e2ex e

  Ident ident -> load $ fst $ env M.! ident

  Let (Def kind bt be) reste -> do
    bv  <- eval' env be
    ind <- storeNew bv
    eval' (M.insert (b2i bt) (ind, kind) env) reste

  Graph bts reste -> eval' env $ nuls $ muts $ reste
    where nul (Def kind bt _) r = e2ex $ Let (Def kind bt nothing) r
          nuls k = foldr nul k bts
          mut (Def _ bt be) r =
            e2ex $ Let (Def Val (i2b seqid) $ e2ex $ Assign (b2i bt) be) r
          muts k = foldr mut k bts

  App f args -> do
    Fun params body <- ex2e <$> eval' env f
    if length args /= length params
      then error "wrong number of args"
      else do
        let ps = zipWith (,) args undefined
        undefined

  Assign ident expr -> case defKind of
    Val -> error "Cannot mutate non-variable"
    Var -> do bv <- eval' env expr
              store ptr bv
              eval' env nothing
    where (ptr, defKind) = env M.! ident

  where nothing = e2ex $ Ident $ mkID "__Nothing"
        seqid   = mkID "__Never_Used"
