module Pyrec.Report where

import Prelude hiding (map, mapM)

import Data.Traversable

import Control.Applicative
import Control.Monad        hiding (mapM)
import Control.Monad.Writer hiding (mapM)

import           Pyrec.IR
import           Pyrec.IR.Parse      (BindN)
import           Pyrec.IR.Desugar    (BindT)
import           Pyrec.IR.Check as C
import qualified Pyrec.IR.Core  as R

type Errors = [R.ErrorMessage]

report :: C.Expr -> (R.Expr, Errors)
report = runWriter . rpt

rpt :: C.Expr -> Writer Errors R.Expr
rpt (E l t e) = case e of

  Num n -> return $ oe $ Num n
  Str s -> return $ oe $ Str s

  Fun bds e        -> fmap oe $ Fun bds <$> rpt e
  Let d e          -> fmap oe $ Let     <$> mapM rpt d         <*> rpt e
  Graph ds e       -> fmap oe $ Graph   <$> mapM (mapM rpt) ds <*> rpt e
  App f as         -> fmap oe $ App     <$> rpt f              <*> mapM rpt as
  Try e1 bd e2     -> fmap oe $ Try     <$> rpt e1
                      <*> return bd <*> rpt e2
  Cases vt v cases -> fmap oe $ Cases vt
                      <$> rpt v <*> mapM (mapM rpt) cases

  Ident (Bound   _ il is) -> return $ oe $ Ident $ R.Bound il is
  Ident (Unbound      is) -> err $ R.UnboundId is

  Assign i v -> case i of
    Bound Var il is -> (oe . Assign (R.Bound il is)) <$> rpt v
    Bound Val il is -> err $ R.MutateVar il is
    Unbound      is -> err $ R.UnboundId is

--  _ -> R.E l t <$> mapM rpt e -- so close....

  where oe e = R.E l t e
        
        err :: R.Error -> Writer Errors R.Expr
        err e = do let e' = (l , e)
                   tell [e']
                   return $ R.Error e'
