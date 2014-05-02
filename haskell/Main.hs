module Main where

import           Control.Applicative
import           Control.Monad.Writer

import qualified Data.Map         as M
import           Data.Map         (Map)

import           Text.Parsec.Pos
import           Text.Parsec.Error

import qualified System.IO        as IO
import qualified System.Exit      as Exit

import           Pyrec
import           Pyrec.Misc

import           Pyrec.IR
import           Pyrec.IR.Desugar as D

import           Pyrec.Report     as R

foreignEnv :: Map String (Decl BindT bn ())
foreignEnv = M.fromList $ fmap (uncurry numBinOp) [
  (1000 , "@pyretPlus"  ),
  (1001 , "@pyretMinus" ),
  (1002 , "@pyretTimes" ),
  (1003 , "@pyretDivide")
  ]
  where numBinOp l n =
          ( n
          , Def Val (D.BT (pos l) n $ T $ TFun [T $ TIdent "Number", T $ TIdent "Number"] $ T $ TIdent "Number") ())

stdEnv = M.fromList $ fmap (uncurry numBinOp) [
  (8000 , "Number"),
  (9000 , "String")
  ]
  where numBinOp l n =
          ( n
          , Def Val (D.BT (pos l) n $ T $ TType) ())

env = M.union foreignEnv stdEnv

main :: IO ()
main = pretty True =<< fmap runWriter . parseEmit env <$> getContents

pretty :: Bool -> Either ParseError (String, Errors) -> IO ()
pretty exit either = case either of
  (Left  err)          -> do
    IO.hPutStrLn IO.stderr $ show err
    when exit Exit.exitFailure

  (Right (llvm, errs)) -> do
    forM_ errs $ IO.hPutStrLn IO.stderr . show
    IO.hPutStr IO.stdout llvm
    when exit Exit.exitSuccess

-- TESTING --
pos = newPos "test" 1 . fromInteger

e l = D.E (pos l) TUnknown

eLet l n v i = Let (Def Val (D.BT (pos l) n TUnknown) v) i
eVar l n v i = Let (Def Var (D.BT (pos l) n TUnknown) v) i
eSeq     v i = Let (Def Val (D.BT (pos 0) "$temp" TUnknown) v) i

prog1 = e 1 $ eLet 2 "x" (e 3 $ Num 55) $ e 2 $ Ident "x"

prog2 = e 1 $ eVar 2 "x" (e 3 $ Num 55)
            $ e 4 $ eSeq (e 6 $ Assign "x" $ e 7 $ Num 37)
                  $ e 8 $ Ident "x"

prog3 = e 1 $ App (e 2 $ Ident "f") $ map (\a -> e (round a) $ Num a) [1..4]
env3  = M.singleton "f" $ Def Val (D.BT (pos 9) "f" $ T $ TFun (replicate 4 $ T $ TIdent "Number") $ T $ TIdent "String") ()

prog4 = e 1 $ eLet 2 "x" (e 3 $ Num 77)
            $ e 4 $ App (e 5 $ Ident "@pyretTimes") [e 6 $ Ident "x", e 7 $ Num 33]
