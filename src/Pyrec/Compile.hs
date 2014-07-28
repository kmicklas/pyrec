module Pyrec.Compile where

import Data.Word

import Control.Monad.State
import Control.Applicative

import qualified Pyrec.IR            as IR
import qualified Pyrec.IR.Desugar    as D
import qualified Pyrec.IR.ScopeCheck as SC
import qualified Pyrec.IR.TypeCheck  as TC
import qualified Pyrec.IR.Core       as R

import           Pyrec.CPS
import           Pyrec.Misc

type CPS = State Word

gen :: CPS Name
gen = (Gen <$> get) <* modify (+ 1)

cpsProgram :: String -> String -> R.Expr -> CPS Expr
cpsProgram exitName exceptName e = cps e (n2v exitName, n2v exceptName)
  where n2v n = Var $ Name n Intrinsic

cps :: R.Expr -> (Val, Val) -> CPS Expr
--cps (R.Error e) (_,  _)  = error $ show e
cps (R.E l t e) (rk, ek) = case e of
  IR.Num n -> return $ k $ Num n
  IR.Str s -> return $ k $ Str s

  IR.Ident (SC.Bound l n) -> return $ k $ Var $ Name n l

  IR.Fun args b -> do
    rk' <- gen
    ek' <- gen
    bv  <- cps b (Var rk', Var ek')

    iden <- gen
    return $ Fix [Fun iden args' (rk', ek') bv] $ k $ Var iden

    where args' = for args $ \ (TC.BT l n _) -> Name n l

  IR.Let decl rest -> do cps (R.E l t $ IR.Graph [decl] rest) (rk, ek)

  IR.App f args -> do
    fv <- gen

    let finalApply :: [Val] -> CPS Expr
        finalApply argVals = return $ App (Var fv) (reverse argVals) (rk, ek)

    cps f =<< (, ek) <$> Cont fv <$> foldl evalArg finalApply args []

    where evalArg :: ([Val] -> CPS Expr) -> R.Expr -> ([Val] -> CPS Expr)
          evalArg k a = \restArgs -> do
            av <- gen
            cps a =<< (, ek) <$> Cont av <$> (k $ Var av : restArgs)

  where k :: Val -> Expr
        k = Continue rk


foldM' base list f = foldM f base list
