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

cpsProgram :: String -> String -> R.Expr -> CPS Expr
cpsProgram exitName exceptName = cps (n2v exitName, n2v exceptName)
  where n2v n = Name n Intrinsic

cps :: (Name, Name) -> R.Expr -> CPS Expr
cps (_,  _)  (R.Error e) = error $ show e
cps (rk, ek) (R.E l t e) = case e of
  IR.Num n -> return $ k $ Num n
  IR.Str s -> return $ k $ Str s

  IR.Ident (R.Bound l n) -> return $ k $ Var $ Name n l

  IR.Fun args b -> do rk' <- gen
                      ek' <- gen
                      f  <- gen
                      b' <- cps (rk', ek') b
                      return $ Fix [Fun f argNames (rk', ek') b'] $ k $ Var f
    where argNames = for args $ \ (C.BT l n _) -> Name n l

  IR.Let decl rest -> cps (rk, ek) $ R.E l t $ IR.Graph [decl] rest

  where k :: Val -> Expr
        k = Cont (Var rk)
