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
import qualified Pyrec.IR.ScopeCheck  as SC
import qualified Pyrec.IR.TypeCheck   as TC
import qualified Pyrec.IR.Core        as R

import           Pyrec.Foreign

import qualified Pyrec.Parse          as P
import qualified Pyrec.Desugar        as D
import qualified Pyrec.ScopeCheck     as S
import qualified Pyrec.TypeCheck      as T
--import qualified Pyrec.Report         as R
import qualified Pyrec.Compile        as C
import qualified Pyrec.LLVM           as L


type Compile a = RWS () [Message R.Error] Word a

lift' :: Maybe a -> Compile a
lift' = \case
  Just a  -> return a
  Nothing -> tell [] >> error "errors in sc or tcq!"

parse :: String -> Either ParseError A.Module
parse = P.parseString P.program

desugar :: A.Module -> Compile D.Expr
desugar = mapRWS f . D.convModule
  where f (a, s, w) = (a, s, R.Earlier <$$> w)

scopeCheck :: T.Env -> D.Expr -> Compile SC.Expr
scopeCheck env e = lift' $ S.scE (trim2 <$> M.mapKeys trim1 env) e
  where trim1 (D.BN _ i) = i
        trim2 = \case (Def dt (SC.BT l _ _) _) -> S.Entry l dt
                      (Data   (D.BN  l _)   _) -> S.Entry l Val

typeCheck :: T.Env -> SC.Expr -> Compile TC.Expr
typeCheck env e = lift' $ T.synth env e

report :: TC.Expr -> Compile R.Expr
report e = return $ conv e
  where conv (TC.E u t e) = R.E u t $ conv <$> e
{-
report e = rws $ \_ s -> let
  (a, w) = runWriter $ R.report e
  in (a, s, w)
-}
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

cumulativeScopeCheck :: String -> Either ParseError (Compile SC.Expr)
cumulativeScopeCheck = fmap (scopeCheck defaultEnv =<<) . cumulativeDesugar

cumulativeTypeCheck :: String -> Either ParseError (Compile TC.Expr)
cumulativeTypeCheck = fmap (typeCheck defaultEnv =<<) . cumulativeScopeCheck

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
