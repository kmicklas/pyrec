module Pyrec.Desugar where

import           Prelude                  hiding (map, mapM)

import           Control.Applicative
import           Control.Monad            hiding (mapM)
import           Control.Monad.Writer     hiding (mapM)

import qualified Data.Map            as M
import           Data.Map                        (Map)
import           Data.Traversable

import           Text.Parsec.Pos

import           Pyrec.AST
import qualified Pyrec.IR            as IR
import qualified Pyrec.IR.Desugar    as D

type DS = Writer [D.ErrorMessage]

convModule :: Module -> DS D.Expr
convModule (Module _ _ b) = convBlock b

convBlock :: Block -> DS D.Expr
convBlock [] = return $ error "empty block"
convBlock [Node p (ExprStmt e)] = convExpr e
convBlock (Node p (ExprStmt e) : rest) =
  convBlock $ Node p (LetStmt (Let (Bind (Node p $ "temp@" ++ show p)
                                         Nothing) e)) : rest
convBlock (Node p (LetStmt (Let bd e)) : rest) =
  D.E p D.TUnknown <$>
    (IR.Let <$> (IR.Def IR.Val <$> convBind bd <*> convExpr e)
            <*> afterDef p rest)
  where afterDef p [] = do tell [(p, D.EndBlockWithDef)]
                           convBlock []
        afterDef p1 rest@(Node p2 _ : _) =
          do if sourceLine p1 == sourceLine p2
             then tell [(p2, D.SameLineStatements)]
             else return ()
             convBlock rest

convBind :: Bind -> DS D.BindT
convBind (Bind (Node p id) Nothing) = return $ D.BT p id D.TUnknown

convExpr :: Node Expr -> DS D.Expr
convExpr (Node p e) = case e of
  Num n          -> return $ mkT IR.TNum $ IR.Num n
  Id (Node _ id) -> return $ mkU $ IR.Ident id
  
  where mk = D.E p
        mkT = mk . D.T
        mkU = mk D.TUnknown
