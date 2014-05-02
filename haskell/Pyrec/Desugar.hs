module Pyrec.Desugar where

import           Prelude                  hiding (map, mapM)

import           Control.Applicative
import           Control.Monad            hiding (mapM)
import           Control.Monad.Writer     hiding (mapM)

import qualified Data.Map            as M
import           Data.Map                        (Map)
import           Data.Traversable

import           Text.Parsec.Pos

import           Pyrec.Misc
import           Pyrec.AST
import qualified Pyrec.IR            as IR
import qualified Pyrec.IR.Desugar    as D

type DS = Writer [D.ErrorMessage]

convModule :: Module -> DS D.Expr
convModule (Module _ _ b) = convBlock b

convBlock :: Block -> DS D.Expr
convBlock stmts = case stmts of
  []                           -> return $ error "empty block"
  [Node p (ExprStmt e)]        -> convExpr e
  (Node p (ExprStmt e) : rest) ->
    convBlock $ Node p (LetStmt (Let (Bind (Node p $ "temp$" ++ showLoc p)
                                           Nothing) e)) : rest
  (Node p (LetStmt (Let bd e)) : rest) ->
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
convBind (Bind (Node p id) ty) = D.BT p id <$> convMaybeType ty

convBN :: Id -> DS D.BindN
convBN (Node p i) = return $ D.BN p i

convType :: Node Type -> DS D.Type
convType = undefined

convMaybeType :: Maybe (Node Type) -> DS D.Type
convMaybeType t = case t of
  Nothing  -> return D.TUnknown
  (Just t) -> convType t

convExpr :: Node Expr -> DS D.Expr
convExpr (Node p e) = case e of
  Num n          -> return $ mkT IR.TNum $ IR.Num n
  Id (Node _ id) -> return $ mkU $ IR.Ident id

  Fun Nothing Nothing Nothing     body -> convBlock $ body
  Fun Nothing Nothing (Just retT) body -> convExpr  $ rebuild $ TypeConstraint (rebuild $ Block body) retT

  Fun (Just tparams) Nothing       retT body -> mkT <$> t <*> body'
    where tparams' = mapM convBN tparams
          t        = IR.TParam <$> tparams' <*> convMaybeType retT
          body'    = IR.FunT <$> tparams' <*> convBlock body

  Fun Nothing        (Just params) retT body -> undefined
    where (types, ids) = unzip $ (\(Bind p i) -> (p, i)) <$> params

  Fun tparams params retT body -> undefined

  where mk = D.E p
        mkT = mk . D.T
        mkU = mk D.TUnknown
        rebuild :: q -> Node q
        rebuild = Node p
