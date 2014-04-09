module Main where

import qualified Data.Map          as M
import           Data.Map               (Map)

import           Pyrec.AST
--import           Pyrec.AST.Parse   as P hiding (Type(T))
import           Pyrec.AST.Desugar as D
import qualified Pyrec.AST.Check   as C

import           Pyrec.Check
import           Pyrec.Report
import           Pyrec.Compile
import           Pyrec.Emit

ffiEnv = M.fromList $ fmap (uncurry numBinOp) [
  (1000 , "@pyretPlus"  ),
  (1001 , "@pyretMinus" ),
  (1002 , "@pyretTimes" ),
  (1003 , "@pyretDivide")
  ]
  where numBinOp l n =
          ( n
          , Def Val (D.BT l n $ T $ TFun [T TNum, T TNum] $ T TNum) ())

checkEmit :: D.Expr -> String
checkEmit e = emit $ compile $ fst $ report $ tc ffiEnv e

main = putStr $ checkEmit prog4

-- TESTING --
e l = D.E l TUnknown

eLet l n v i = Let (Def Val (D.BT l n TUnknown) v) i
eVar l n v i = Let (Def Var (D.BT l n TUnknown) v) i
eSeq     v i = Let (Def Val (D.BT 0 "$temp" TUnknown) v) i

prog1 = e 1 $ eLet 2 "x" (e 3 $ Num 55) $ e 2 $ Ident "x"

prog2 = e 1 $ eVar 2 "x" (e 3 $ Num 55)
            $ e 4 $ eSeq (e 6 $ Assign "x" $ e 7 $ Num 37)
                  $ e 8 $ Ident "x"

prog3 = e 1 $ App (e 2 $ Ident "f") $ map (\a -> e (round a) $ Num a) [1..4]
env3  = M.singleton "f" $ Def Val (D.BT 9 "f" $ T $ TFun (replicate 4 $ T TNum) $ T TStr) ()

prog4 = e 1 $ eLet 2 "x" (e 3 $ Num 77)
            $ e 4 $ App (e 5 $ Ident "@pyretTimes") [e 6 $ Ident "x", e 7 $ Num 33]
