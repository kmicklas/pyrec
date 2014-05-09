module Pyrec.Desugar where

import           Prelude                  hiding (map, mapM)

import           Control.Applicative
import           Control.Monad            hiding (mapM)
import           Control.Monad.RWS        hiding (mapM, sequence)

import qualified Data.Map            as M
import           Data.Map                        (Map)
import           Data.Word
import           Data.Traversable         hiding (sequence)

import           Text.Parsec.Pos

import           Pyrec.Misc
import           Pyrec.Error
import           Pyrec.AST
import qualified Pyrec.IR            as IR
import qualified Pyrec.IR.Desugar    as D

type DS = RWS () [D.ErrorMessage] Word

gen :: DS Word
gen = get <* modify (+1)

mkUnique :: SourcePos -> DS Unique
mkUnique p = User p <$> gen

convModule :: Module -> DS D.Expr
convModule (Module _ _ b) = convBlock b

convBlock :: Block -> DS D.Expr
convBlock (Statements stmts) = case stmts of
  []                             -> return $ D.E undefined (D.T $ IR.TIdent "Nothing") $ IR.Ident "nothing"
  (Node p (LetStmt _let) : rest) -> letCommon IR.Val p _let rest
  (Node p (VarStmt _let) : rest) -> letCommon IR.Var p _let rest
  [Node p e]                     -> convExpr $ Node p e
  (Node p e              : rest) ->
    recur $ Node p (LetStmt (Let (Bind (Node p $ "temp$" ++ showLoc p) Nothing)
                                 (Node p e))) : rest

  where recur = convBlock . Statements

        letCommon vv p (Let bd e) rest = do
          uniq <- mkUnique p
          D.E uniq D.TUnknown <$>
            (IR.Let <$> (IR.Def vv <$> convBind bd <*> convExpr e)
                    <*> afterDef uniq rest)
          where afterDef p [] = do tell [Msg p D.EndBlockWithDef]
                                   recur []
                afterDef p1 rest@(Node p2 _ : _) =
                  do u2 <- mkUnique p2
                     when (sameLine p1 u2) $
                       tell [Msg u2 D.SameLineStatements]
                     recur rest
                sameLine :: Unique -> Unique -> Bool
                sameLine (User a _ ) (User b _ ) = sourceLine a == sourceLine b
                sameLine _           _           = False


convBind :: Bind Id -> DS D.BindT
convBind (Bind (Node p id) ty) = D.BT <$> mkUnique p <*> pure id <*> convMaybeType ty

convBN :: Id -> DS D.BindN
convBN (Node p i) = D.BN <$> mkUnique p <*> pure i

convType :: Node Type -> DS D.Type
convType (Node p t) = case t of
  TIdent  (Node _ name) -> return $ D.T $ IR.TIdent name
  TFun    tys ty        -> fmap     D.T $ IR.TFun    <$> (mapM recur  tys) <*> recur ty
  TParam  ids ty        -> fmap     D.T $ IR.TParam  <$> (mapM convBN ids) <*> recur ty
  TObject binds         -> fmap     D.T $ IR.TObject <$> M.fromList <$> mapM getPair binds
  where recur = convType . Node p
        getPair :: Bind Id-> DS (IR.FieldName, D.Type)
        getPair (Bind (Node _ id) ty) = (,) <$> return id <*> convMaybeType ty

convMaybeType :: Maybe (Node Type) -> DS D.Type
convMaybeType t = case t of
  Nothing  -> return D.TUnknown
  (Just t) -> convType t

convExpr :: Node Expr -> DS D.Expr
convExpr (Node p e) = case e of
  Str s             -> mkT <*> (pure $ IR.TIdent "String") <*> (pure $ IR.Str s)
  Num n             -> mkT <*> (pure $ IR.TIdent "Number") <*> (pure $ IR.Num n)
  Ident (Node _ id) -> mkU <*> (pure $ IR.Ident id)

  Fun Nothing Nothing Nothing Nothing     body -> convBlock $ body
  Fun Nothing Nothing Nothing (Just retT) body ->
    convExpr  $ rebuild $ TypeConstraint (rebuild $ Block body) retT

  Fun (Just tparams) Nothing Nothing retT body -> mkT <*> t <*> body'
    where tparams' = mapM convBN tparams
          t        = IR.TParam <$> tparams' <*> convMaybeType retT
          body'    = IR.FunT   <$> tparams' <*> convBlock     body

  Fun Nothing Nothing (Just params) retT body -> mkT <*> t <*> body'
    where binds    = mapM convBind params
          types    = (\(D.BT _ _ t) -> t) <$$> binds
          t        = IR.TFun <$> types <*> convMaybeType retT
          body'    = IR.Fun  <$> binds <*> convBlock     body

  Fun tparams Nothing params retT body ->
    convExpr $ rebuild $ Fun tparams Nothing Nothing Nothing
    $ Statements $ [rebuild $ Fun Nothing Nothing params retT body]

  App  f args -> (mkU <*>) $ IR.App  <$> convExpr f <*> sequence (convExpr <$> args)
  AppT f args -> (mkU <*>) $ IR.AppT <$> convExpr f <*> sequence (convType <$> args)

  UnOp  tok e           -> convExpr $ rebuild $ App (Node p $ Ident $ rebuild $ tok) [e]
  BinOp e  []           -> error "empty binop chain"
  BinOp e1 [(tok,e2)]   -> convExpr $ rebuild $ App (Node p $ Ident $ rebuild $ tok) [e1, e2]
  BinOp e1 ((tok,e2):r) -> convExpr $ rebuild $ App (Node p $ Ident $ rebuild $ tok) [e1, rebuild $ BinOp e2 r]

  TypeConstraint expr typ -> D.Constraint <$> mkUnique p <*> convType typ <*> convExpr expr

  where mk  = D.E <$> mkUnique p
        mkT = fmap (. D.T) mk
        mkU = mk <*> pure D.TUnknown
        rebuild :: q -> Node q
        rebuild = Node p
