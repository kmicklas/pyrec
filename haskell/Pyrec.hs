module Pyrec where

import           Control.Monad.Writer

import           Text.Parsec.Error

import qualified Pyrec.AST        as A

import qualified Pyrec.IR.Desugar as D
import qualified Pyrec.IR.Core    as R

import qualified Pyrec.Parse      as P
import qualified Pyrec.Desugar    as D
import qualified Pyrec.Check      as C
import qualified Pyrec.Report     as R
import qualified Pyrec.Compile    as O
import qualified Pyrec.Emit       as E

infixr 9 <.>
(<.>) :: Functor f => (a1 -> b) -> (a -> f a1) -> a -> f b
a <.> b = fmap a . b

checkEmit :: C.Env -> D.Expr -> Writer R.Errors String
checkEmit foreignEnv = E.emit <.> O.compile <.> R.report . C.tc foreignEnv

desugarEmit :: C.Env -> A.Module -> Writer R.Errors String
desugarEmit foreignEnv = checkEmit foreignEnv <=< (mapWriter $ fmap . fmap . fmap $ R.Earlier) . D.convModule

parseEmit :: C.Env -> String -> Either ParseError (Writer R.Errors String)
parseEmit foreignEnv = desugarEmit foreignEnv <.> P.parseString P.program