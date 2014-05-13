{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
module Main
  ( main
  ) where

import           System.Console.CmdLib

import           PyrecDriver.Error
import           PyrecDriver.Commands

main :: IO ()
main = do args <-getArgs
          (flip run' args :: Arguments -> IO ()) =<< dispatchR [] args

data Arguments
  = Lint
  | LlvmAst
  | Bitcode
  deriving (Typeable, Data, Eq)

instance Attributes Arguments where
  attributes _ = noAttributes

instance RecordCommand Arguments where
  mode_summary = \case
    Lint    -> "print warnings and discard IR"
    LlvmAst -> "print out the LLVM AST"
    Bitcode -> "dump (to stdout) a object file for linking"

  run' cmd _ = topCatchError $ case cmd of
    Lint    -> dumpWarnings
    LlvmAst -> dumpLLVM_AST
    Bitcode -> dumpBitcode
