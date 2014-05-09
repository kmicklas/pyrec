{-# LANGUAGE FlexibleInstances #-}

module Pyrec.TypeCheckSpec where

import           Prelude                  hiding (map, mapM)

import           Control.Applicative
import           Control.Monad            hiding (mapM, forM)
import           Control.Monad.Writer     hiding (mapM, forM, sequence)

import qualified Data.Map                 as M
import           Data.Map                 (Map)
import           Data.Word
import           Data.Traversable         hiding (for, sequence)

import           Text.Parsec.Error

import           Control.Monad            (mzero)

import           System.FilePath          hiding ((<.>))
import           System.Directory         (getDirectoryContents)
import           System.IO.Unsafe         as Sin

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
import           Pyrec.IR.Check   as C
import qualified Pyrec.IR.Core    as R

import qualified Pyrec.Parse      as P
import qualified Pyrec.Desugar    as D
import           Pyrec.ScopeCheck as S
import           Pyrec.TypeCheck  as T
import qualified Pyrec.Report     as R
import qualified Pyrec.Compile    as O
import qualified Pyrec.Emit       as E

strip (D.E          l t e) = D.E l t $ strip <$> e
strip (D.Constraint _ _ e) = strip e

pd :: String -> Either ParseError (D.Expr, [D.ErrorMessage])
pd = runWriter <.> parseDesugar

testInfer :: T.Env -> String -> Either ParseError (Bool, R.Expr, R.Expr,
                                                   [R.ErrorMessage],
                                                   [R.ErrorMessage],
                                                   [R.ErrorMessage])
testInfer env src = for (pd src) $ \case
  (e, errors) -> (e1' == e2', e1', e2', errors', e1r, e2r)
    where errors'    = R.Earlier <$$> errors
          (e1', e1r) = runWriter $ checkReport env e
          (e2', e2r) = runWriter $ checkReport env $ strip e

noErrors, fillInConstraints ::
  Either ParseError (Bool, R.Expr, R.Expr,
                     [R.ErrorMessage],
                     [R.ErrorMessage],
                     [R.ErrorMessage]) -> Bool
noErrors = \case
  (Right (_, _, _, [], [], [])) -> True
  _                             -> False

fillInConstraints = \case
  (Right (b, _, _, [], [], [])) -> b
  _                             -> False


spec :: Spec
spec = unifySpec >> tcSpec

unifySpec = describe "the type-unifier" $ do

  it "combines identical types without modification" $
    property $ within 5000000 $ \(ty :: C.Type) -> unify M.empty ty ty == ty

tcSpec = describe "the type checker" $ do

  it "type checks natural number literals" $
    property $ \(num :: Word) ->
    fillInConstraints $ testInfer env $ "(" ++ show num ++ " :: Number)"

  it "type checks decimal literals" $
    property $ \(n1 :: Word) (n2 :: Word) ->
    fillInConstraints $ testInfer env
    $ "(" ++ show n1 ++ "." ++ show n2 ++ " :: Number)"

  it "type checks string literals" $
    property $ \(num :: String) -> fillInConstraints $ testInfer env $ "(" ++ show num ++ " :: String)"

  it "accepts the identity function" $
    testInfer env "fun<A>(x :: A): x;" `shouldSatisfy` noErrors

  it "accepts a concrete \"infinite loop\"" $
    testInfer env "fun bob(x :: String) -> A: bob(x);" `shouldSatisfy` noErrors

  it "accepts a polymorphic \"infinite loop\"" $
    testInfer env "fun bob<A>(x :: A) -> A: bob<A>(x);" `shouldSatisfy` noErrors

  testFiles "tests/error" "catches bad program"
    (testInfer env) (not . noErrors)

  testFiles "tests/fill-in" "fills in the removed constraints"
    (testInfer env) fillInConstraints

-- hopefully an upstream change will pave the way for my atonement
testFiles :: Show a => FilePath -> String -> (String -> a) -> (a -> Bool) -> Spec
testFiles directory msg function predicate = forM_ files $ \fileName ->
  it (msg ++ ": " ++ fileName) $ do
    contents <- readFile $ directory </> fileName
    function contents `shouldSatisfy` predicate

  where files :: [FilePath]
        files = filter (/= ".") $ filter (/= "..")
                $ Sin.unsafePerformIO $ getDirectoryContents directory
