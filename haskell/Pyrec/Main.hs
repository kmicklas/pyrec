module Pyrec.Main where

import qualified Data.Map as M
import           Data.Map (Map)

import           Pyrec.AST
import           Pyrec.AST.Parse as P
import qualified Pyrec.AST.Check as C

import           Pyrec.Check
import           Pyrec.Report
import           Pyrec.Compile
import           Pyrec.Emit

checkEmit :: P.Expr -> String
checkEmit e = emit $ compile $ snd $ report $ tc emptyEnv e

-- TESTING --
e l = P.E l TUnknown

eLet l n v i = Let (Def Val (P.B l n TUnknown) v) i
eVar l n v i = Let (Def Var (P.B l n TUnknown) v) i
eSeq     v i = Let (Def Val (P.B 0 "%temp" TUnknown) v) i

prog1 = e 1 $ eLet 2 "x" (e 3 $ Num 55) $ e 2 $ Ident "x"

prog2 = e 1 $ eVar 2 "x" (e 3 $ Num 55)
            $ e 4 $ eSeq (e 6 $ Assign "x" $ e 7 $ Num 37)
                  $ e 8 $ Ident "x"

prog3 = e 1 $ App (e 2 $ Ident "f") $ map (\a -> e (round a) $ Num a) [1..4]
env3  = M.singleton "f" $ Def Val (P.B 9 "f" $ T $ TFun (replicate 4 $ T TNum) $ T TStr) ()
