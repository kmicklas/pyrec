module Pyrec.Foreign where

import           Prelude              hiding (map, mapM)

import           Control.Applicative

import qualified Data.Map             as M
import           Data.Map                    (Map)
import           Data.Traversable     hiding (for, sequence)
import           Data.Word

import           Pyrec.Misc

import           Pyrec.IR

import qualified Pyrec.IR.Desugar     as D
import qualified Pyrec.IR.Check       as C
import qualified Pyrec.IR.Core        as R

import qualified Pyrec.TypeCheck      as T

types :: T.Env
types = M.fromList $ numBinOp <$> ["Number", "String"]
  where numBinOp n = ( D.BN Intrinsic n
                     , Def Val (C.BT Intrinsic n $ C.T $ TType) ())

funs :: T.Env
funs = M.fromList $ numBinOp <$> [ "@pyretPlus",  "@pyretMinus"
                                 , "@pyretTimes", "@pyretDivide"]
  where numBinOp n =
          ( D.BN Intrinsic n
          , Def Val (C.BT Intrinsic n $ C.T $ TFun [ numT, numT] $ numT) ())
        numT = C.T $ TIdent $ C.Bound Val Intrinsic "Number"

defaultEnv = M.union types funs
