module Pyrec.Report where

import           Prelude                  hiding (map, mapM)

import           Control.Applicative
import           Control.Monad            hiding (mapM)
import           Control.Monad.Writer     hiding (mapM)

import           Data.Traversable

import           Pyrec.IR
import           Pyrec.IR.Desugar                (BindN, BindT)
import           Pyrec.IR.Check       as C
import qualified Pyrec.IR.Core        as R

type Errors = [R.ErrorMessage]
type RP     = Writer Errors

report :: C.Expr -> RP R.Expr
report (E l t e) = case e of

  Num n -> return $ oe $ Num n
  Str s -> return $ oe $ Str s

  Fun bds e        -> fmap oe $ Fun bds <$> report e
  Let d e          -> fmap oe $ Let     <$> mapM report d         <*> report e
  Graph ds e       -> fmap oe $ Graph   <$> mapM (mapM report) ds <*> report e
  App f as         -> fmap oe $ App     <$> report f              <*> mapM report as
  Try e1 bd e2     -> fmap oe $ Try     <$> report e1
                      <*> return bd <*> report e2
  Cases vt v cases -> fmap oe $ Cases vt
                      <$> report v <*> mapM (mapM report) cases

  Ident (Bound   _ il is) -> return $ oe $ Ident $ R.Bound il is
  Ident (Unbound      is) -> err $ R.UnboundId is

  Assign i v -> case i of
    Bound Var il is -> (oe . Assign (R.Bound il is)) <$> report v
    Bound Val il is -> err $ R.MutateVar il is
    Unbound      is -> err $ R.UnboundId is

--  _ -> R.E l t <$> mapM report e -- so close....

  where oe e = R.E l t e

        err :: R.Error -> RP R.Expr
        err e = do let e' = (l , e)
                   tell [e']
                   return $ R.Error e'
