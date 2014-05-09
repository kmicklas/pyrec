module Pyrec.Compile where

import Data.Word

import Control.Monad.State
import Control.Applicative

import qualified Pyrec.IR         as IR
import qualified Pyrec.IR.Core    as R
import qualified Pyrec.IR.Check   as C
import qualified Pyrec.IR.Desugar as D

import Pyrec.CPS
import Pyrec.Misc

type CPS = State Word

gen :: CPS Name
gen = (Gen <$> get) <* modify (+ 1)

cps :: (Name, Name) -> R.Expr -> CPS Expr
cps (rc, ec) (R.E _ _ e) = case e of
  IR.Num n -> return $ k $ Num n
  IR.Str s -> return $ k $ Str s

  IR.Ident (R.Bound l n) -> return $ k $ Var $ Name n l

  IR.Fun args b -> do rc' <- gen
                      ec' <- gen
                      f  <- gen
                      b' <- cps (rc', ec') b
                      return $ Fix [Fun f argNames (rc', ec') b'] $ k $ Var f
    where argNames = for args $ \ (C.BT l n _) -> Name n l

  where k :: Val -> Expr
        k = Cont (Var rc)
