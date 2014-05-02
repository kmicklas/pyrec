module Pyrec where

import           Prelude                  hiding (map, mapM)

import           Control.Applicative
import           Control.Monad            hiding (mapM)
import           Control.Monad.Writer     hiding (mapM, sequence)

import           Data.Traversable         hiding (for, sequence)

import           Text.Parsec.Error

import           Pyrec.Misc
import           Pyrec.Error

import qualified Pyrec.AST        as A

import qualified Pyrec.IR.Desugar as D
import qualified Pyrec.IR.Core    as R

import qualified Pyrec.Parse      as P
import qualified Pyrec.Desugar    as D
import qualified Pyrec.Check      as C
import qualified Pyrec.Report     as R
import qualified Pyrec.Compile    as O
import qualified Pyrec.Emit       as E

compileEmit :: R.Expr -> String
compileEmit = E.emit . O.compile

checkReport :: C.Env -> D.Expr -> R.RP R.Expr
checkReport foreignEnv = R.report . C.tc foreignEnv

parseDesugar :: String -> Either ParseError (D.DS D.Expr)
parseDesugar = D.convModule <.> P.parseString P.program


parseEmit :: C.Env -> String -> Either ParseError (Writer R.Errors String)
parseEmit foreignEnv =
  (compileEmit <.> checkReport foreignEnv <=<
   (mapWriter $ fmap . fmap . fmap $ R.Earlier)) <.> parseDesugar


testInfer :: C.Env -> String
             -> Either ParseError
             (Bool, R.Expr, R.Expr, [R.ErrorMessage], [R.ErrorMessage], [R.ErrorMessage])
testInfer env src = for desugared $ \(e1, errors) -> let
  errors'    = (fmap . fmap) R.Earlier errors
  (e1', e1r) = runWriter $ checkReport env e1
  (e2', e2r) = runWriter $ checkReport env e1
  in (e1' == e2', e1', e2', errors', e1r, e2r)
  where desugared :: Either ParseError (D.Expr, [D.ErrorMessage])
        desugared = runWriter <$> parseDesugar src

        strip (D.E          l t e) = D.E l t $ strip <$> e
        strip (D.Constraint _ _ e) = strip e
