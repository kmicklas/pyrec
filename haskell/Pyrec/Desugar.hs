module Pyrec.Desugar where

import           Prelude                  hiding (map, mapM)

import           Control.Applicative
import           Control.Monad            hiding (mapM)
import           Control.Monad.RWS        hiding (mapM)

import qualified Data.Map            as M
import           Data.Map                        (Map)
import           Data.Traversable

import           Pyrec.AST
import qualified Pyrec.IR            as IR
import qualified Pyrec.IR.Desugar    as D

type Entry = IR.Decl D.BindT D.BindN ()
type Env   = Map Id Entry
type DS    = RWS Env [D.ErrorMessage] ()

convBlock :: Block -> DS D.Expr
convBlock = undefined

convExpr :: Node Expr -> DS D.Expr
convExpr (Node p e) = case e of
  Num n -> return $ mkT IR.TNum $ IR.Num n

  where mk = D.E p
        mkT = mk . D.T
        mkU = mk D.TUnknown
