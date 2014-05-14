module PyrecDriver.Commands where

import           Control.Monad.Error

import qualified Data.ByteString          as BS

import qualified System.IO                as IO

import           LLVM.General.AST         as AST
import           LLVM.General.PrettyPrint

import           LLVM.General.Context
import           LLVM.General.Module      as Mod
import           LLVM.General.Target

import           Pyrec
import           Pyrec.Misc
import           Pyrec.Error
import           Pyrec.PrettyPrint

import           PyrecDriver.Error

putInErrorT :: (Show e, Functor n, Monad n)
           => (a -> Either e b) -> a -> ErrorT String n b
putInErrorT c = mapErrorT (fmap showE) . ErrorT . return . c
 where showE = \case Right a -> Right a
                     Left  e -> Left $ show e

-- helpers, with IO continuation k

compileDumpWarnings  :: (AST.Module -> DriverError r) -> DriverError r
compileDumpWarnings k = do
  (warnings, user) <- putInErrorT compile =<< lift IO.getContents
  lift $ forM_ warnings $ IO.hPutStrLn IO.stderr . pp

  k user

getModule :: (Context -> Mod.Module -> DriverError r) -> DriverError r
getModule k = compileDumpWarnings $ \user -> do
  liftHigher withContext $ \context -> do
    liftHigherJoin (withModuleFromAST context user) $ k context

link :: (Context -> Mod.Module -> ErrorT String IO r) -> DriverError r
link k = getModule $ \context user -> do
  let with = withModuleFromBitcode context $ File "runtime.bc"
  liftHigherJoin with $ \runtime -> do
    linkModules False user runtime -- mutates user :/
    k context user

-- commands

dumpWarnings, dumpLLVM_AST, dumpObjectFile :: DriverError ()

dumpWarnings = compileDumpWarnings $ const $ mzero

dumpCPS = do
  (warnings, user) <- putInErrorT cumulativeCPS' =<< lift IO.getContents
  lift $ forM_ warnings $ IO.hPutStrLn IO.stderr . pp
  lift $ IO.hPutStr IO.stdout $ show user

dumpLLVM_General = compileDumpWarnings $ \user ->
  lift $ IO.hPutStr IO.stdout $ showPretty user

dumpLLVM_AST = getModule $ \_ user -> do
  progString <- lift $ moduleLLVMAssembly user
  lift $ IO.hPutStr IO.stdout $ progString

dumpObjectFile = getModule $ \_ linked -> do
  liftHigherJoin withDefaultTargetMachine $ \machine -> do
    object <- moduleObject machine linked
    lift $ BS.hPutStr IO.stdout $ object
