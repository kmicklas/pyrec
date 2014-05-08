module Main where

import           Control.Applicative
import           Control.Monad.Writer

import           Text.Parsec.Error

import qualified System.IO        as IO
import qualified System.Exit      as Exit

import           Pyrec
import           Pyrec.PrettyPrint

import           Pyrec.Report     as R

main :: IO ()
main = pretty True =<< fmap runWriter . parseEmit env <$> getContents

pretty :: Bool -> Either ParseError (String, Errors) -> IO ()
pretty exit either = case either of
  (Left  err)          -> do
    IO.hPutStrLn IO.stderr $ show err
    when exit Exit.exitFailure

  (Right (llvm, errs)) -> do
    forM_ errs $ IO.hPutStrLn IO.stderr . pp
    IO.hPutStr IO.stdout llvm
    when exit Exit.exitSuccess
