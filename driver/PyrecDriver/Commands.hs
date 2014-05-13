module PyrecDriver.Commands where

import           Control.Monad.Error

import qualified Data.ByteString          as BS

import qualified System.IO                as IO

import           LLVM.General.AST         as AST

import           LLVM.General.Context
import           LLVM.General.Module      as Mod
import           LLVM.General.Target

import           Pyrec
import           Pyrec.PrettyPrint

import           PyrecDriver.Error


compile' :: String
         -> DriverError ([Message Pyrec.Error], AST.Module)
compile' = mapErrorT (fmap showE) . ErrorT . return . compile
 where showE = \case Right a -> Right a
                     Left  e -> Left $ show e

-- helpers, with IO continuation k

compileDumpWarnings  :: (AST.Module -> DriverError r) -> DriverError r
compileDumpWarnings k = do
  (warnings, user) <- compile' =<< lift IO.getContents
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

dumpLLVM_AST = getModule $ \_ user -> do
  progString <- lift $ moduleLLVMAssembly user
  lift $ IO.hPutStr IO.stdout $ progString

dumpObjectFile = link $ \_ linked -> do
  liftHigherJoin withDefaultTargetMachine $ \machine -> do
    object <- moduleObject machine linked
    lift $ BS.hPutStr IO.stdout $ object
