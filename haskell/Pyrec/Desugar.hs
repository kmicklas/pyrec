module Pyrec.Desugar where

import           Control.Monad.RWS
import           Data.Map               (Map)
import qualified Data.Map         as M

import           Pyrec.AST
import qualified Pyrec.IR         as IR
import qualified Pyrec.IR.Desugar as D

type Entry = IR.Decl D.BindT D.BindN ()

type Env = Map Id Entry

type DS = RWS (Map String Entry) [D.ErrorMessage] ()

convBlock :: Block -> DS D.Expr
convBlock = undefined

convExpr :: Node Expr -> DS D.Expr
convExpr (Node p e) = case e of
  Num n -> return $ mkT IR.TNum $ IR.Num n

  where mk = D.E p
        mkT = mk . D.T
        mkU = mk D.TUnknown
