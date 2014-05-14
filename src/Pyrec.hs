module Pyrec
  ( Message
  , R.Error

  , module Pyrec.Foreign

  , parse
  , desugar
  , scopeCheck
  , typeCheck
  , report
  , cps
  , llvm

  , cumulativeDesugar
  , cumulativeScopeCheck
  , cumulativeTypeCheck
  , cumulativeReport
  , cumulativeCPS
  , cumulativeCPS'
  , cumulativeLLVM

  , compile
  ) where

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

import           Pyrec.Foreign

import qualified Pyrec.Parse          as P
import qualified Pyrec.Desugar        as D
import qualified Pyrec.ScopeCheck     as S
import qualified Pyrec.TypeCheck      as T
import qualified Pyrec.Report         as R
import qualified Pyrec.Compile        as C
import qualified Pyrec.LLVM           as L


type Compile a = RWS () [Message R.Error] Word a


parse :: String -> Either ParseError A.Module
parse = P.parseString P.program

desugar :: A.Module -> Compile D.Expr
desugar = mapRWS f . D.convModule
  where f (a, s, w) = (a, s, R.Earlier <$$> w)

scopeCheck :: T.Env -> D.Expr -> C.Expr
scopeCheck env = S.scE $ trim2 <$> M.mapKeys trim1 env
  where trim1 (D.BN _ i) = i
        trim2 = \case (Def dt (C.BT l _ _) _) -> S.Entry l dt
                      (Data   (D.BN l _)   _) -> S.Entry l Val

typeCheck :: T.Env -> C.Expr -> C.Expr
typeCheck = T.tc

report :: C.Expr -> Compile R.Expr
report e = rws $ \_ s -> let
  (a, w) = runWriter $ R.report e
  in (a, s, w)

cps :: R.Expr -> Compile K.Expr
cps e = rws $ \_ s -> let
  (a, s') = runState (C.cpsProgram "Return" "Except" e) s
  in (a, s', mempty)

llvm :: K.Expr -> Compile LLVM.General.AST.Module
llvm e = do
  s <- get
  return $ L.llvmModule s "Pyret" "userMain" runtimeDecls e



cumulativeDesugar :: String -> Either ParseError (Compile D.Expr)
cumulativeDesugar = desugar <.> parse

cumulativeScopeCheck :: String -> Either ParseError (Compile C.Expr)
cumulativeScopeCheck = scopeCheck defaultEnv <..> cumulativeDesugar

cumulativeTypeCheck :: String -> Either ParseError (Compile C.Expr)
cumulativeTypeCheck = typeCheck defaultEnv <..> cumulativeScopeCheck

cumulativeReport :: String -> Either ParseError (Compile R.Expr)
cumulativeReport = fmap (report =<<) . cumulativeTypeCheck

cumulativeCPS :: String -> Either ParseError (Compile K.Expr)
cumulativeCPS  = fmap (cps =<<) . cumulativeReport

cumulativeCPS' = extract <.> cumulativeCPS

cumulativeLLVM :: String -> Either ParseError (Compile LLVM.General.AST.Module)
cumulativeLLVM = fmap (llvm =<<) . cumulativeCPS

compile :: String -> Either ParseError ([Message R.Error], Module)
compile = extract <.> cumulativeLLVM

extract m = (warnings, mod)
  where (mod, _, warnings) = runRWS m () 0
