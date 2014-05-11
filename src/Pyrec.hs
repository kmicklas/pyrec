module Pyrec where

import           Prelude              hiding (map, mapM)

import           Control.Applicative
import           Control.Monad        hiding (mapM)
import           Control.Monad.Writer hiding (mapM, sequence)
import           Control.Monad.State  hiding (mapM, sequence)
import           Control.Monad.RWS    hiding (mapM, sequence)

import qualified Data.Map             as M
import           Data.Map                    (Map)
import           Data.Traversable     hiding (for, sequence)
import           Data.Word

import           Text.Parsec.Pos
import           Text.Parsec.Error    hiding (Message)

import           LLVM.General.AST            (Module)

import           Pyrec.Misc
import           Pyrec.Error
import           Pyrec.PrettyPrint

import qualified Pyrec.AST            as A
import qualified Pyrec.CPS            as K

import           Pyrec.IR

import qualified Pyrec.IR.Desugar     as D
import qualified Pyrec.IR.Check       as C
import qualified Pyrec.IR.Core        as R

import qualified Pyrec.Parse          as P
import qualified Pyrec.Desugar        as D
import qualified Pyrec.ScopeCheck     as S
import qualified Pyrec.TypeCheck      as T
import qualified Pyrec.Report         as R
import qualified Pyrec.Compile        as C
import qualified Pyrec.LLVM           as L


type PyrecMonad a = RWS () [Message R.Error] Word a


foreignFuns :: T.Env
foreignFuns = M.fromList $ numBinOp <$> [ "@pyretPlus",  "@pyretMinus"
                                        , "@pyretTimes", "@pyretDivide"]
  where numBinOp n =
          ( D.BN Intrinsic n
          , Def Val (C.BT Intrinsic n $ C.T $ TFun [ numT, numT] $ numT) ())
        numT = C.T $ TIdent $ C.Bound Val Intrinsic "Number"

foreignTypes :: T.Env
foreignTypes = M.fromList $ numBinOp <$> ["Number", "String"]
  where numBinOp n = ( D.BN Intrinsic n
                     , Def Val (C.BT Intrinsic n $ C.T $ TType) ())

defaultEnv = M.union foreignFuns foreignTypes


parse :: String -> Either ParseError A.Module
parse = P.parseString P.program

desugar :: A.Module -> PyrecMonad D.Expr
desugar = mapRWS f . D.convModule
  where f (a, s, w) = (a, s, R.Earlier <$$> w)

scopeCheck :: T.Env -> D.Expr -> C.Expr
scopeCheck env = S.scE $ trim2 <$> M.mapKeys trim1 env
  where trim1 (D.BN _ i) = i
        trim2 = \case (Def dt (C.BT l _ _) _) -> S.Entry l dt
                      (Data   (D.BN l _)   _) -> S.Entry l Val

typeCheck :: T.Env -> C.Expr -> C.Expr
typeCheck = T.tc

report :: C.Expr -> PyrecMonad R.Expr
report e = rws $ \_ s -> let
  (a, w) = runWriter $ R.report e
  in (a, s, w)

cps :: R.Expr -> PyrecMonad K.Expr
cps e = rws $ \_ s -> let
  (a, s') = runState (C.cps (_, _) e) s
  in (a, s', mempty)

llvm :: K.Expr -> LLVM.General.AST.Module
llvm = L.llvmModule


pyrec :: String -> Either ParseError (PyrecMonad Module)
pyrec = desugarEmit <.> parse
  where desugarEmit = llvm <.> cps <=< report
                      . typeCheck defaultEnv
                      . scopeCheck defaultEnv
                      <=< desugar
