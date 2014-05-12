module Main where

import           Control.Applicative
import           Control.Monad.RWS
import           Control.Monad.Error


import qualified System.IO         as IO
import qualified System.Exit       as Exit

import           LLVM.General.AST
import           LLVM.General.PrettyPrint

import           LLVM.General.Context
import           LLVM.General.Module

import           Text.Parsec.Error hiding (Message)

import           Pyrec
import           Pyrec.PrettyPrint

import           Pyrec.Report      as R

displayError :: ErrorT String IO () -> IO ()
displayError = runErrorT >=> \case
  Left errors -> do
    IO.hPutStrLn IO.stderr errors
    Exit.exitFailure
  Right _     -> Exit.exitSuccess

showErrors :: Show e => Either e a -> Either String a
showErrors = \case
  Right a -> Right a
  Left  e -> Left $ show e

main :: IO ()
main = displayError $ do
  (llvmAST, warnings) <- mapErrorT (fmap showErrors)
                         $ ErrorT $ compile <$> getContents

  lift $ forM_ warnings $ IO.hPutStrLn IO.stderr . pp

  ErrorT $ withContext $ \context -> runErrorT $ do
    progString <- withModuleFromAST context llvmAST moduleLLVMAssembly
    lift $ IO.hPutStr IO.stdout $ progString
    return ()
