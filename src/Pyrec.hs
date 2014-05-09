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
import qualified Pyrec.IR.Check   as C
import qualified Pyrec.IR.Core    as R

import qualified Pyrec.Parse      as P
import qualified Pyrec.Desugar    as D
import qualified Pyrec.ScopeCheck as S
import qualified Pyrec.TypeCheck  as T
import qualified Pyrec.Report     as R
import qualified Pyrec.Compile    as O
import qualified Pyrec.Emit       as E


pos = newPos "test" 1 . fromInteger

foreignEnv :: T.Env
foreignEnv = M.fromList $ numBinOp <$> [ "@pyretPlus",  "@pyretMinus"
                                       , "@pyretTimes", "@pyretDivide"]
  where numBinOp n =
          ( D.BN Intrinsic n
          , Def Val (C.BT Intrinsic n $ C.T $ TFun [ numT, numT] $ numT) ())
        numT = C.T $ TIdent $ C.Bound Val Intrinsic "Number"

stdEnv :: T.Env
stdEnv = M.fromList $ numBinOp <$> ["Number", "String"]
  where numBinOp n =
          ( D.BN Intrinsic n
          , Def Val (C.BT Intrinsic n $ C.T $ TType) ())

env = M.union foreignEnv stdEnv



compileEmit :: R.Expr -> String
compileEmit = E.emit . O.compile

checkReport :: T.Env -> D.Expr -> R.RP R.Expr
checkReport env =  R.report . T.tc env
                          . (S.scE $ trim2 <$> M.mapKeys trim1 env)
  where trim1 (D.BN _ i) = i
        trim2 = \case (Def dt (C.BT l _ _) _) -> S.Entry l dt
                      (Data   (D.BN l _)   _) -> S.Entry l Val

parseDesugar :: String -> Either ParseError (D.DS D.Expr)
parseDesugar = D.convModule <.> P.parseString P.program


parseEmit :: T.Env -> String -> Either ParseError (Writer R.Errors String)
parseEmit env =
  (compileEmit <.> checkReport env <=<
   (mapWriter $ fmap . fmap . fmap $ R.Earlier)) <.> parseDesugar

