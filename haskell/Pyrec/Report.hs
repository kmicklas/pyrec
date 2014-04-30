module Pyrec.Report where

import           Prelude                  hiding (map, mapM, sequence)

import           Control.Applicative
import           Control.Monad            hiding (mapM, sequence)
import           Control.Monad.Writer     hiding (mapM, sequence)

import           Data.Traversable

import           Pyrec.IR
import qualified Pyrec.IR.Desugar     as D
import           Pyrec.IR.Check       as C
import qualified Pyrec.IR.Core        as R

type Errors = [R.ErrorMessage]
type RP     = Writer Errors

report :: C.Expr -> RP R.Expr
report = rpIdent <=< rpTypeError

-- | looks for shadowed identifiers
rpShadow :: C.Expr -> RP C.Expr
rpShadow = undefined

-- | looks for identifiers bound twice 'concurrently', e.g:
-- |  - identifier names when pattern matching
-- |  - constructor names in one ADT declaration
-- |  - field names in one variant
-- |    (redundant field names in record is impossible)
-- | this is much more serious than 'normal' shadowing
rpDup :: C.Expr -> RP C.Expr
rpDup = undefined

-- need to recur on recursive types
-- | reports type errors
rpTypeError :: C.Expr -> RP C.Expr
rpTypeError e@(E l t inner) = case t of
  (D.TError     terror) -> err $ R.TypeError  terror
  (D.PartialObj fields) -> err $ R.PartialObj fields
  _                     -> E l t <$> mapM rpTypeError inner
  where err error = tell [(l, error)] >> return e

-- | reports errors involving mutation and unbound identifiers
rpIdent :: C.Expr -> RP R.Expr
rpIdent (E l t e) = case e of

  Num n -> return $ oe $ Num n
  Str s -> return $ oe $ Str s

  Fun bds e        -> fmap oe $ Fun bds  <$> rp e
  Let d e          -> fmap oe $ Let      <$> mapM rp d           <*> rp e
  Graph ds e       -> fmap oe $ Graph    <$> (mapM . mapM) rp ds <*> rp e
  App f as         -> fmap oe $ App      <$> rp f                <*> mapM rp as
  Try e1 bd e2     -> fmap oe $ Try      <$> rp e1
                      <*> return bd <*> rp e2
  Cases vt v cases -> fmap oe $ Cases vt <$> rp v <*> (mapM . mapM) rp cases

  Ident (Bound   _ il is) -> return $ oe $ Ident $ R.Bound il is
  Ident (Unbound      is) -> err $ R.UnboundId is

  Assign i v -> case i of
    Bound Var il is -> oe <$> Assign (R.Bound il is) <$> rp v
    Bound Val il is -> err $ R.MutateVar il is
    Unbound      is -> err $ R.UnboundId is

  EmptyObject    -> return $ oe EmptyObject
  Extend o fi fv -> fmap oe $ Extend <$> rp o <*> return fi <*> rp fv
  Access o fi    -> fmap oe $ Access <$> rp o <*> return fi

--  _ -> R.E l t <$> mapM rp e -- so close....

  where rp = rpIdent
        oe e = R.E l t e

        err :: R.Error -> RP R.Expr
        err e = tell [e'] >> (return $ R.Error e')
          where e' = (l , e)
