module Pyrec.Report where

import           Prelude                  hiding (map, mapM, sequence)

import           Control.Applicative
import           Control.Monad            hiding (mapM, sequence)
import           Control.Monad.Writer     hiding (mapM, sequence)

import           Data.Traversable         hiding (for)
import           Data.Foldable

import           Pyrec.Misc
import           Pyrec.Error

import           Pyrec.IR
import qualified Pyrec.IR.Desugar     as D
import           Pyrec.IR.Desugar                (BindN(..))
import qualified Pyrec.IR.ScopeCheck  as SC
import           Pyrec.IR.ScopeCheck             (Id(..))
import           Pyrec.IR.TypeCheck   as TC
import qualified Pyrec.IR.Core        as R

type Errors = [R.ErrorMessage]
type RP     = Writer Errors

-- | We use this to find errors in dead code too
data ExprWithErrors i
  = EE Unique TC.Type [R.Error] (Pyrec.IR.Expr BindT BindN i TC.Type (ExprWithErrors i))
  deriving (Eq, Show)

report :: TC.Expr -> RP R.Expr
report = trim . convID . rpIdent . rpTypeError . prepare


foldExpr :: (ExprWithErrors i -> Pyrec.IR.Expr BindT BindN i TC.Type e -> e)
              -> ExprWithErrors i -> e
foldExpr f e@(EE _ _ _ inner) = f e $ foldExpr f <$> inner

-- | Adds an extra field to each node to accumulate errors
prepare :: TC.Expr -> ExprWithErrors Id
prepare (E l t e) = EE l t [] $ prepare <$> e

-- | Turns nodes with errors into "bombs"
-- | Reports errors
trim :: ExprWithErrors R.Id -> RP R.Expr
trim = foldExpr $ \(EE l t errors _) inner' -> case errors of
  [] -> R.E l t <$> sequence inner'
  _  -> do let errMsgs = for errors $ \a -> Msg l a
           tell errMsgs
           _ <- sequence inner' -- report errors from further down the treex
           return $ R.Error $ errMsgs

-- | Adds more errors to a node
err :: ExprWithErrors i -> [R.Error] -> ExprWithErrors i
err (EE l t errors expr) errors' = EE l t (errors ++ errors') expr

-- | Updates a node with bombs
merge :: ExprWithErrors ignored
         -> Pyrec.IR.Expr BindT BindN i TC.Type (ExprWithErrors i)
         -> ExprWithErrors i
merge (EE l t errors _) inner' = EE l t errors inner'



-- | looks for shadowed identifiers
rpShadow :: ExprWithErrors Id -> ExprWithErrors R.Id
rpShadow = _

-- | looks for identifiers bound twice 'concurrently', e.g:
-- |  - identifier names when pattern matching
-- |  - constructor names in one ADT declaration
-- |  - field names in one variant
-- |    (redundant field names in record is impossible)
-- | this is much more serious than 'normal' shadowing
rpDup :: ExprWithErrors Id -> ExprWithErrors Id
rpDup = _

-- | reports type errors
rpTypeError :: ExprWithErrors SC.Id -> ExprWithErrors SC.Id
rpTypeError = foldExpr $ \e@(EE _ t _ _) rest ->
  err (merge e rest) $ (R.TypeError t) <$> getTypeErrors t
  where
    getTypeErrors :: TC.Type -> [R.TypeError]
    -- | deconstructs the type, accumulating errors
    getTypeErrors t = case t of
      TC.TUnknown          -> err $ R.AmbiguousType
      TC.PartialObj fields -> err $ R.PartialObj fields
      TC.TError     terror -> err $ R.TEEarlier  terror
      TC.T t               -> foldMap getTypeErrors t
      where err a = [a]

rpIdent :: ExprWithErrors SC.Id -> ExprWithErrors SC.Id
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
convID :: ExprWithErrors SC.Id -> ExprWithErrors R.Id
convID (EE l t errors e) = EE l t errors $ case e of

  Num n -> Num n
  Str s -> Str s

  Fun  bds e       -> Fun  bds $ r e
  FunT pts e       -> FunT pts $ r e
  Let d e          -> Let      (r <$> d)   $ r e
  Graph ds e       -> Graph    (r <$$> ds) $ r e
  App  f as        -> App      (r f)       $ r <$> as
  AppT f ts        -> AppT     (r f)         ts
  Try e1 bd e2     -> Try      (r e1) bd   $ r e2
  Cases vt v cases -> Cases vt (r v)       $ r <$$> cases

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
