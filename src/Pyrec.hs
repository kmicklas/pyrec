module Pyrec where

import           Prelude                  hiding (map, mapM)

import           Control.Applicative
import           Control.Monad            hiding (mapM)
import           Control.Monad.Writer     hiding (mapM, sequence)

import qualified Data.Map                 as M
import           Data.Map                 (Map)

import           Data.Traversable         hiding (for, sequence)

import           Text.Parsec.Pos
import           Text.Parsec.Error

import           Pyrec.Misc
import           Pyrec.Error
import           Pyrec.PrettyPrint

import qualified Pyrec.AST        as A

import           Pyrec.IR

import qualified Pyrec.IR.Desugar as D
import qualified Pyrec.IR.Core    as R

import qualified Pyrec.Parse      as P
import qualified Pyrec.Desugar    as D
import qualified Pyrec.Check      as C
import qualified Pyrec.Report     as R
import qualified Pyrec.Compile    as O
import qualified Pyrec.Emit       as E


pos = newPos "test" 1 . fromInteger

foreignEnv :: Map String (Decl D.BindT bn ())
foreignEnv = M.fromList $ fmap (uncurry numBinOp) [
  (1000 , "@pyretPlus"  ),
  (1001 , "@pyretMinus" ),
  (1002 , "@pyretTimes" ),
  (1003 , "@pyretDivide")
  ]
  where numBinOp l n =
          ( n
          , Def Val (D.BT (pos l) n $ D.T $ TFun [D.T $ TIdent "Number", D.T $ TIdent "Number"] $ D.T $ TIdent "Number") ())

stdEnv = M.fromList $ fmap (uncurry numBinOp) [
  (8000 , "Number"),
  (9000 , "String")
  ]
  where numBinOp l n =
          ( n
          , Def Val (D.BT (pos l) n $ D.T $ TType) ())

env = M.union foreignEnv stdEnv



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

