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

liftHigher :: Control.Monad.Error.Error e
               => ((a -> IO (Either e c)) -> IO (Either e c))
               -> (a -> ErrorT e IO c)
               -> ErrorT e IO c
liftHigher with f = ErrorT $ with $ runErrorT . f

liftHigherJoin :: Control.Monad.Error.Error e
               => ((a -> IO (Either e c)) -> ErrorT e IO (Either e c))
               -> (a -> ErrorT e IO c)
               -> ErrorT e IO c
liftHigherJoin with f = ErrorT $ fmap join $ runErrorT $ with $ runErrorT . f

main :: IO ()
main = displayError $ do
  (user, warnings) <- mapErrorT (fmap showErrors)
                      $ ErrorT $ compile <$> getContents

  lift $ forM_ warnings $ IO.hPutStrLn IO.stderr . pp

  liftHigher withContext $ \context -> do
    liftHigherJoin (withModuleFromAST context user) $ \user -> do
--      liftHigherJoin (withModuleFromAST context rumtime) $ \runtime -> do
--      linkModules False user runtime -- mutates user :/
      progString <- lift $ moduleLLVMAssembly user
      lift $ IO.hPutStr IO.stdout $ progString
