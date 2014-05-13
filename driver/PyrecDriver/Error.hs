module PyrecDriver.Error where

import           Control.Monad.Error

import qualified System.IO         as IO
import qualified System.Exit       as Exit

type DriverError a = ErrorT String IO a

liftHigher :: Error e
               => ((a -> IO (Either e c)) -> IO (Either e c))
               -> (a -> ErrorT e IO c)
               -> ErrorT e IO c
liftHigher with f = ErrorT $ with $ runErrorT . f

liftHigherJoin :: Error e
               => ((a -> IO (Either e c)) -> ErrorT e IO (Either e c))
               -> (a -> ErrorT e IO c)
               -> ErrorT e IO c
liftHigherJoin with f = ErrorT $ fmap join $ runErrorT $ with $ runErrorT . f

topCatchError :: ErrorT String IO () -> IO ()
topCatchError = runErrorT >=> \case
  Left errors -> do
    IO.hPutStrLn IO.stderr $ errors
    Exit.exitFailure
  Right _     -> Exit.exitSuccess