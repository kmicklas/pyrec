{-# LANGUAGE FlexibleInstances #-}

module Pyrec.CheckSpec where

import           Prelude                  hiding (map, mapM)

import           Control.Applicative
import           Control.Monad            hiding (mapM)
import           Control.Monad.Writer     hiding (mapM, sequence)

import           Data.Traversable         hiding (for, sequence)

import           Text.Parsec.Error

import           Control.Monad            (mzero)

import           Test.Hspec
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck          hiding ((.&.))

import           Pyrec.Misc
import           Pyrec.Error
import           Pyrec.PrettyPrint

import           Pyrec

import qualified Pyrec.AST        as A

import           Pyrec.IR

import qualified Pyrec.IR.Desugar as D
import qualified Pyrec.IR.Core    as R

import qualified Pyrec.Parse      as P
import qualified Pyrec.Desugar    as D
import qualified Pyrec.Check      as C
import qualified Pyrec.Report     as R
import qualified Pyrec.Compile    as O
import qualified Pyrec.Emit       as E


{-

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
-}

strip (D.E          l t e) = D.E l t $ strip <$> e
strip (D.Constraint _ _ e) = strip e

pd :: String -> Either ParseError (D.Expr, [D.ErrorMessage])
pd = runWriter <.> parseDesugar

testInfer :: C.Env -> String -> Either ParseError (Bool, R.Expr, R.Expr,
                                                   [R.ErrorMessage],
                                                   [R.ErrorMessage],
                                                   [R.ErrorMessage])
testInfer env src = for (pd src) $ \case
  (e, errors) -> (e1' == e2', e1', e2', errors', e1r, e2r)
    where errors'    = (fmap . fmap) R.Earlier errors
          (e1', e1r) = runWriter $ checkReport env e
          (e2', e2r) = runWriter $ checkReport env $ strip e

noErrors ::
  Either ParseError (Bool, R.Expr, R.Expr,
                     [R.ErrorMessage],
                     [R.ErrorMessage],
                     [R.ErrorMessage]) -> Bool
noErrors = \case
  (Right (_, _, _, [], [], [])) -> True
  _                             -> False

fillsOutConstraints = \case
  (Right (b, _, _, [], [], [])) -> b
  _                          -> False


instance Show R.Expr where
  show = pp

instance Show (Pyrec.Error.Message R.Error) where
  show = pp

spec :: Spec
spec = do
  describe "the type checker" $ do

    it "type checks number literals" $
      property $ \(num :: Double) -> fillsOutConstraints $ testInfer env $ "(" ++ show num ++ " :: Number)"

    it "type checks string literals" $
      property $ \(num :: String) -> fillsOutConstraints $ testInfer env $ "(" ++ show num ++ " :: String)"

    it "accepts the identity function" $
      testInfer env "fun<A>(x :: A): x;" `shouldSatisfy` noErrors

    it "accepts a concrete \"infinite loop\"" $
      testInfer env "fun bob(x :: String) -> A: bob(x);" `shouldSatisfy` noErrors

    it "accepts a polymorphic \"infinite loop\"" $
      testInfer env "fun bob<A>(x :: A) -> A: bob<A>(x);" `shouldSatisfy` noErrors
