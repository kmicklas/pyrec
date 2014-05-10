module Pyrec where

import           Prelude                  hiding (map, mapM)

import           Control.Applicative
import           Control.Monad            hiding (mapM)
import           Control.Monad.Writer     hiding (mapM, sequence)
import           Control.Monad.RWS        hiding (mapM, sequence)

import qualified Data.Map                 as M
import           Data.Map                 (Map)
import           Data.Traversable         hiding (for, sequence)
import           Data.Word

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

foreignFuns :: T.Env
foreignFuns = M.fromList $ numBinOp <$> [ "@pyretPlus",  "@pyretMinus"
                                       , "@pyretTimes", "@pyretDivide"]
  where numBinOp n =
          ( D.BN Intrinsic n
          , Def Val (C.BT Intrinsic n $ C.T $ TFun [ numT, numT] $ numT) ())
        numT = C.T $ TIdent $ C.Bound Val Intrinsic "Number"

foreignTypes :: T.Env
foreignTypes = M.fromList $ numBinOp <$> ["Number", "String"]
  where numBinOp n =
          ( D.BN Intrinsic n
          , Def Val (C.BT Intrinsic n $ C.T $ TType) ())

env = M.union foreignFuns foreignTypes



compileEmit :: R.Expr -> String
compileEmit = _ . _

checkReport :: D.Expr -> R.RP R.Expr
checkReport =  R.report . T.tc env
               . (S.scE $ trim2 <$> M.mapKeys trim1 env)
  where trim1 (D.BN _ i) = i
        trim2 = \case (Def dt (C.BT l _ _) _) -> S.Entry l dt
                      (Data   (D.BN l _)   _) -> S.Entry l Val

parseDesugar :: String -> Either ParseError (D.DS D.Expr)
parseDesugar = D.convModule <.> P.parseString P.program

parseEmit :: String -> Either ParseError (RWS () R.Errors Word D.Expr)
parseEmit = g
--  (compileEmit <.> checkReport env <=< (mapRWS f)) <.> parseDesugar
  where f (a, s, w) = (a, s, R.Earlier <$$> w)
        g = mapRWS f <.> parseDesugar
