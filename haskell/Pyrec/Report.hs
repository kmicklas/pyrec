module Pyrec.Report where

import           Prelude                  hiding (map, mapM, sequence)

import           Control.Applicative
import           Control.Monad            hiding (mapM, sequence)
import           Control.Monad.Writer     hiding (mapM, sequence)

import           Data.Traversable
import           Data.Foldable

import           Pyrec.Misc

import           Pyrec.IR
import qualified Pyrec.IR.Desugar     as D
import           Pyrec.IR.Check       as C
import qualified Pyrec.IR.Core        as R

type Errors = [R.ErrorMessage]
type RP     = Writer Errors


report :: C.Expr -> RP R.Expr
report = trim . convID . rpIdent . rpTypeError . prepare


foldExpr :: (R.ExprWithErrors i -> Pyrec.IR.Expr D.BindT D.BindN i D.Type e -> e)
              -> R.ExprWithErrors i -> e
foldExpr f e@(R.EE _ _ _ inner) = f e $ foldExpr f <$> inner

-- | Adds an extra field to each node to accumulate errors
prepare :: C.Expr -> R.ExprWithErrors Id
prepare (E l t e) = R.EE l t [] $ prepare <$> e

-- | Turns nodes with errors into "bombs"
-- | Reports errors
trim :: R.ExprWithErrors R.Id -> RP R.Expr
trim = foldExpr $ \(R.EE l t errors _) inner' -> case errors of
  [] -> R.E l t <$> sequence inner'
  _  -> do let errMsgs = fmap (\a -> (l,a)) errors
           tell errMsgs
           return $ R.Error $ errMsgs

-- | Adds more errors to a node
err :: R.ExprWithErrors i -> [R.Error] -> R.ExprWithErrors i
err (R.EE l t errors expr) errors' = R.EE l t (errors ++ errors') expr

-- | Updates a node with bombs
merge :: R.ExprWithErrors ignored
         -> Pyrec.IR.Expr D.BindT D.BindN i D.Type (R.ExprWithErrors i)
         -> R.ExprWithErrors i
merge (R.EE l t errors _) inner' = R.EE l t errors inner'



-- | looks for shadowed identifiers
rpShadow :: R.ExprWithErrors Id -> R.ExprWithErrors Id
rpShadow = undefined

-- | looks for identifiers bound twice 'concurrently', e.g:
-- |  - identifier names when pattern matching
-- |  - constructor names in one ADT declaration
-- |  - field names in one variant
-- |    (redundant field names in record is impossible)
-- | this is much more serious than 'normal' shadowing
rpDup :: R.ExprWithErrors Id -> R.ExprWithErrors Id
rpDup = undefined

-- | reports type errors
rpTypeError :: R.ExprWithErrors C.Id -> R.ExprWithErrors C.Id
rpTypeError = foldExpr $ \e@(R.EE _ t _ _) rest ->
  err (merge e rest) $ fmap (R.TypeError t) $ getTypeErrors t
  where
    getTypeErrors :: D.Type -> [R.TypeError]
    -- | deconstructs the type, accumulating errors
    getTypeErrors t = case t of
      D.TUnknown          -> err $ R.AmbiguousType
      D.PartialObj fields -> err $ R.PartialObj fields
      D.TError     terror -> err $ R.TEEarlier  terror
      D.T t               -> foldMap getTypeErrors t
      where err a = [a]

rpIdent :: R.ExprWithErrors C.Id -> R.ExprWithErrors C.Id
rpIdent = foldExpr $ \old rest ->
  err (merge old rest) $ case rest of
    Ident (Unbound i) -> [R.UnboundId i]

    Assign i _        -> case i of
      Bound Val l s -> [R.MutateVar l s]
      Unbound     s -> [R.UnboundId s]
      _             -> []

    _                 -> []


-- | converts Id type
-- needs more boilplate cause can only derive functor for one parameter :(
convID :: R.ExprWithErrors C.Id -> R.ExprWithErrors R.Id
convID (R.EE l t errors e) = R.EE l t errors $ case e of

  Num n -> Num n
  Str s -> Str s

  Fun bds e        -> Fun bds $ r e
  Let d e          -> Let      (fmap r d)           $ r e
  Graph ds e       -> Graph    ((fmap . fmap) r ds) $ r e
  App f as         -> App      (r f)                $ fmap r as
  Try e1 bd e2     -> Try      (r e1)                 bd $  r e2
  Cases vt v cases -> Cases vt (r v) $ (fmap . fmap) r cases

  Ident (Bound   _ il is) -> Ident $ R.Bound il is
  Ident (Unbound      is) -> Ident $ R.Bound l is
                             -- wrong but will be trimmed

  Assign i v      -> case i of
    Bound _  il is        -> Assign (R.Bound il is) $ r v
                             -- wrong, when val, but will be trimmed
    Unbound     is        -> Assign (R.Bound  l is) $ r v
                             -- wrong but will be trimmed

  EmptyObject     -> EmptyObject
  Extend o fi fv  -> Extend (r o) fi $ r fv
  Access o fi     -> Access (r o) fi

  where r = convID
