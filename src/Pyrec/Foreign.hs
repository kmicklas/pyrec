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
import           Pyrec.LLVM           as L

import LLVM.General.AST hiding (Name)
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant
import LLVM.General.AST.Float
import LLVM.General.AST.Linkage
import LLVM.General.AST.Visibility

types :: T.Env
types = M.fromList $ numBinOp <$> ["Number", "String"]
  where numBinOp n = ( D.BN Intrinsic n
                     , Def Val (C.BT Intrinsic n $ C.T $ TType) ())

fnames = [ "+", "-", "*", "/"]

funs :: T.Env
funs = M.fromList $ numBinOp <$> fnames
  where numBinOp n =
          ( D.BN Intrinsic n
          , Def Val (C.BT Intrinsic n $ C.T $ TFun [ numT, numT] $ numT) ())
        numT = C.T $ TIdent $ C.Bound Val Intrinsic "Number"

defaultEnv = M.union types funs

runtimeDecls :: [Definition]
runtimeDecls = arith ++ rts ++ [trampInit, trampAdjust]

  where arith = binOp <$> ("pyrec"++) <$> fnames
        rts   = (function "pyrecLoadNumber"
                    [Parameter (FloatingPointType 64 IEEE) (lname "num") []]
                    pVal)
                : (function "pyrecLoadString" [ pyrecArg "string"] pVal)
                : (rtsFun <$> ("pyrec"++) <$> [ "Return", "Except"] )

        rtsFun name = function name [ pyrecArg "val" ] VoidType
        binOp  name = function name [ Parameter (ptr pVal) (lname "env") []
                                    , pyrecArg "rk", pyrecArg "ek"
                                    , pyrecArg "a",  pyrecArg "b" ] VoidType
        trampInit = function "llvm.init.trampoline" [ pyrecArg "tramp"
                                                    , pyrecArg "f"
                                                    , pyrecArg "env" ] VoidType
        trampAdjust = function "llvm.adjust.trampoline" [ pyrecArg "tramp" ] pVal
        pyrecArg name = Parameter pVal (lname name) []
        function name args return = GlobalDefinition
                                    $ Function External
                                               Default
                                               pyrecConvention
                                               []
                                               return
                                               (lname name)
                                               (args, False)
                                               []
                                               Nothing
                                               0
                                               Nothing
                                               []
