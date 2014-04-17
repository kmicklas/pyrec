module Main where

import qualified Data.Map          as M
import           Data.Map               (Map)

import           Text.Parsec.Pos

import           Pyrec.IR
--import           Pyrec.AST.Parse   as P hiding (Type(T))
import           Pyrec.IR.Desugar as D
import qualified Pyrec.IR.Check   as C

import           Pyrec.Check
import           Pyrec.Report
import           Pyrec.Compile
import           Pyrec.Emit

ffiEnv = M.fromList $ fmap (uncurry numBinOp) [
  (pos 1000 , "@pyretPlus"  ),
  (pos 1001 , "@pyretMinus" ),
  (pos 1002 , "@pyretTimes" ),
  (pos 1003 , "@pyretDivide")
  ]
  where numBinOp l n =
          ( n
          , Def Val (D.BT l n $ T $ TFun [T TNum, T TNum] $ T TNum) ())

checkEmit :: D.Expr -> String
checkEmit e = emit $ compile $ fst $ report $ tc ffiEnv e

main = putStr $ checkEmit prog4

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
env3  = M.singleton "f" $ Def Val (D.BT (pos 9) "f" $ T $ TFun (replicate 4 $ T TNum) $ T TStr) ()

prog4 = e 1 $ eLet 2 "x" (e 3 $ Num 77)
            $ e 4 $ App (e 5 $ Ident "@pyretTimes") [e 6 $ Ident "x", e 7 $ Num 33]
