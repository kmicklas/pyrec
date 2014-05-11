module Main where

import           Control.Applicative
import           Control.Monad.RWS

import qualified System.IO         as IO
import qualified System.Exit       as Exit

import           LLVM.General.AST
import           LLVM.General.PrettyPrint

import           Text.Parsec.Error hiding (Message)

import           Pyrec
import           Pyrec.PrettyPrint

import           Pyrec.Report      as R

main :: IO ()
main = prettyPrint True =<< compile <$> getContents

prettyPrint :: Bool -> Either ParseError (Module, [Message Error]) -> IO ()
prettyPrint exit either = case either of
  (Left  err)          -> do
    IO.hPutStrLn IO.stderr $ show err
    when exit Exit.exitFailure

  (Right (llvm, errs)) -> do
    forM_ errs $ IO.hPutStrLn IO.stderr . pp
    IO.hPutStr IO.stdout $ showPretty llvm
    when exit Exit.exitSuccess
