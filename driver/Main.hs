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
  | Cps
  | LlvmGeneral
  | LlvmAst
  | ObjectFile
  deriving (Typeable, Data, Eq)

instance Attributes Arguments where
  attributes _ = noAttributes

instance RecordCommand Arguments where
  mode_summary = \case
    Lint        -> "print warnings and discard IR"
    Cps         -> "print the CPS'd IR"
    LlvmGeneral -> "print AST in Haskell syntax"
    LlvmAst     -> "print out the LLVM AST"
    ObjectFile  -> "dump (to stdout) a object file for linking"

  run' cmd _ = topCatchError $ case cmd of
    Lint        -> dumpWarnings
    Cps         -> dumpCPS
    LlvmGeneral -> dumpLLVM_General
    LlvmAst     -> dumpLLVM_AST
    ObjectFile  -> dumpObjectFile
