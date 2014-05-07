{-# LANGUAGE FlexibleInstances #-}
module Pyrec.PrettyPrint where

import           Prelude             hiding (lines)

import           Control.Applicative

import           Data.List           hiding (lines)
import qualified Data.Map            as M
import           Data.Map                   (Map)
import           Data.Maybe                 (fromMaybe)

import           Text.Parsec.Pos

import           Pyrec.Misc

import           Pyrec.AST           as AST
import qualified Pyrec.IR            as IR

import qualified Pyrec.IR.Desugar    as D
import qualified Pyrec.IR.Check      as C
import qualified Pyrec.IR.Core       as R

parenList :: [String] -> String
parenList l = "("  ++ intercalate ", " l ++ ")"

angleList :: [String] -> String
angleList l = "<"  ++ intercalate ", " l ++ ">"

curlyList :: [String] -> String
curlyList l = "<"  ++ intercalate ", " l ++ ">"

lines :: [String] -> String
lines  [] = ": ;"
lines [l] = ": " ++ show l ++ ";"
lines   l = ":\n" ++ (concat $ fmap ("\n  "++) l) ++ "end"

opt :: Show n => Maybe n -> String
opt Nothing  = ""
opt (Just i) = show i


instance Show n => Show (Node n) where
  show (Node _ n) = show n

instance Show (Bind Id) where
  show (Bind id ty) = show id ++ opt (show <$> ty)

instance Show Statement where
  show = \case
    (ExprStmt   e)   ->  show e
    (LetStmt letd)   ->           show letd
    (VarStmt letd)   -> "var " ++ show letd
    (AssignStmt i e) -> show i ++ " := " ++ show e
    (Graph decls)    -> "graph:" ++ show decls

    (FunStmt tps id ps ret body) ->
      "fun"
      ++ (opt $ angleList <$> fmap show <$> tps)
      ++ " " ++ show id
      ++ (opt $ parenList <$> fmap show <$> ps)
      ++ (opt $ (" -> " ++) . show <$> ret) ++ ":" ++ show body

    (Data id params variants) ->
      "data " ++ show id
      ++ (opt $ angleList <$> fmap show <$> params) ++ ":"
      ++ (lines $ show <$> variants)

instance Show Block where
  show (Statements  stmts) = lines $ fmap show stmts

instance Show Variant where
  show (Variant id binds) = "|  " ++ show id ++ (parenList $ show <$> binds)

instance Show Type where
  show = \case
    (TIdent id)         -> show id
    (TFun   params ret) -> (parenList $ show <$> params) ++ show ret
    (TParam params ret) -> (angleList $ show <$> params) ++ show ret
    (TObject fields)    -> curlyList $ show <$> fields

instance Show (Let Id)  where
  show (Let bind expr) = show bind ++ " = " ++ show expr

instance Show Expr where
  show = \case
    (Num   d)   -> show d
    (Str   s)   -> show s
    (Ident i)   -> show i

    (App  e es) -> (show e ++) $ parenList $ show <$> es
    (AppT e ts) -> (show e ++) $ angleList $ show <$> ts

    (Fun tps ps retT body) -> "fun"
                              ++ (opt $ angleList <$> fmap show <$> tps)
                              ++ (opt $ parenList <$> fmap show <$> ps)
                              ++ (opt $ (" -> " ++) . show <$> retT) ++ ":" ++ show body

    (Block block)          -> "block:" ++ show block
    (TypeConstraint e t)   -> "(" ++ show e ++ " :: " ++ show t ++ ")"


instance Show Module where
  show (Module provide imports block) = show provide
                                        ++ "\n\n" ++ (unlines $ show <$> imports)
                                        ++ "\n\n" ++ show block

instance Show Provide where  show _ = ""
instance Show Import where  show _ = ""


dmn q = Node (newPos "derp-dummy" 0 0) q

mkFunctions ::
  forall bt bn id ty ex
  .  ((bt -> Bind Id)                                       -> (ty -> Type)                  -> bt -> Bind Id)
  -> ((bn -> Id)                                                                          -> bn -> Id)
  -> ((id -> Id)                                                                          -> id -> Id)
  -> ((ty -> Type)  -> (IR.Type bn id ty       -> Type)  -> (ex -> Expr) -> (ex -> Block) -> ty -> Type)
  -> ((ex -> Expr)  -> (IR.Expr bt bn id ty ex -> Expr)  -> (ty -> Type) -> (ex -> Block) -> ex -> Expr)
  -> ((ex -> Block) -> (IR.Expr bt bn id ty ex -> Block) -> (ty -> Type) -> (ex -> Expr)  -> ex -> Block)
  -> ( ex -> Expr, ex -> Block, ty -> Type)
mkFunctions bt bn id ty ex bl = (ex', bl', ty')
  where
    bt' = bt   bt'  ty'
    bn' = bn   bn'
    id' = id   id'
    ty' = ty   ty'  resugarT ex' bl'
    ex' = ex   ex'  resugarE ty' bl'
    bl' = bl   bl'  resugarB ty' ex'

    resugarE :: IR.Expr bt bn id ty ex -> Expr
    resugarE = \case
      (IR.Num n)     -> Num n
      (IR.Str s)     -> Str s
      (IR.Ident i)   -> Ident $ id' i
      (IR.Fun  bs e) -> Fun Nothing             (Just $ bt' <$> bs) Nothing $ bl' e
      (IR.FunT bs e) -> Fun (Just $ bn' <$> bs) Nothing             Nothing $ bl' e

      (IR.App  f args) -> App  (dmn $ ex' f) $ dmn <$> ex' <$> args
      (IR.AppT f args) -> AppT (dmn $ ex' f) $ dmn <$> ty' <$> args

      (IR.Cases _ _ _) -> undefined

      (IR.Try _ _ _)      -> undefined
      (IR.EmptyObject)    -> undefined
      (IR.Extend obj f v) -> undefined
      (IR.Access obj f)   -> undefined

      e@(IR.Let   _ _)   -> Block $ resugarB e
      e@(IR.Graph _ _)   -> Block $ resugarB e
      e@(IR.Assign _ _)  -> Block $ resugarB e

    resugarB :: IR.Expr bt bn id ty ex -> Block
    resugarB = Statements . \case
      (IR.Let d r)       -> convDecl d                                            : bl'' r
      (IR.Graph decls r) -> (dmn $ Graph $ Statements $ convDecl <$> decls) : bl'' r
      (IR.Assign i e)    -> [dmn $ AssignStmt (id' i) $ dmn $ ex' e]

      e                  -> [dmn $ ExprStmt $ dmn $ resugarE e]
      where bl'' = (\case (Statements stmts) -> stmts) . bl'

    convDecl :: IR.Decl bt bn ex -> Node Statement
    convDecl = dmn . \case
      (IR.Data b vs)   -> Data (bn' b) Nothing $ variants <$> vs
      (IR.Def  dt b v) -> k $ Let (bt' b) (dmn $ ex' v)
        where k = case dt of IR.Val -> LetStmt
                             IR.Var -> VarStmt

    variants :: IR.Variant bt bn -> Variant
    variants (IR.Variant b ps) = Variant (bn' b) $ bt' <$> fromMaybe [] ps

    resugarT :: IR.Type bn id ty -> Type
    resugarT = \case
      (IR.TIdent i)          -> TIdent $ id' i
      (IR.TFun   types  ret) -> TFun   (ty' <$> types)  $ ty' ret
      (IR.TParam params ret) -> TParam (bn'  <$> params) $ ty' ret

      (IR.TType)             -> TIdent $ dmn "__TYPE"

      (IR.TObject fieldMap)  -> TObject $ for (M.toList fieldMap) $ \(i,ty) ->
        Bind (dmn i) $ Just $ dmn $ ty' ty

    --type resugE = IR.Expr bt bn id ty ex -> Expr
    --type resugB = IR.Expr bt bn id ty ex -> Block


(convDE, convDB, convDT) =
  mkFunctions
    (\_  cT       (D.BT l i t) -> Bind (Node l i) $ Just $ dmn $ cT t)
    (\_           (D.BN l i)   -> Node l i)
    (\_           i            -> dmn i)
    (\_  qT _  _  (D.T t)      -> qT t)
    (\cE qE cT _               -> \case
        (D.E _ D.TUnknown e) -> qE e
        (D.E _ t          e) -> TypeConstraint (dmn $ qE e) $ dmn $ cT t
        (D.Constraint _ t e) -> TypeConstraint (dmn $ cE e) $ dmn $ cT t)
    (\_  qB _ cE               -> \case
        (D.E _ _ e)          -> qB e
        e                    -> Statements $ [dmn $ ExprStmt $ dmn $ cE e])

instance Show D.Expr where show = show . convDB
instance Show D.Type where show = show . convDT

(convCE, convCB, _) =
  mkFunctions
    (\_  _        (D.BT l i t) -> Bind (Node l i) $ Just $ dmn $ convDT t)
    (\_           (D.BN l i)   -> Node l i)
    (\_           i            -> dmn $ C.getId i)
    (\_  _  _  _  t            -> convDT t) -- D.T uses D.Id, for better or worse
    (\_  qE _  _               -> \case
        (C.E _ D.TUnknown e) -> qE e
        (C.E _ t          e) -> TypeConstraint (dmn $ qE e) $ dmn $ convDT t)
    (\_  qB _ cE               -> \case
        (C.E _ _ e)          -> qB e
        e                    -> Statements $ [dmn $ ExprStmt $ dmn $ cE e])

instance Show C.Expr where show = show . convCB

(convRE, convRB, _) =
  mkFunctions
    (\_  _        (D.BT    l i t) -> Bind (Node l i) $ Just $ dmn $ convDT t)
    (\_           (D.BN    l i)   -> Node l i)
    (\_           (R.Bound l i)   -> Node l i)
    (\_  _  _  _  t               -> convDT t) -- D.T uses D.Id, for better or worse
    (\_  qE _  _                  -> \case
        (R.E _ D.TUnknown e) -> qE e
        (R.E _ t          e) -> TypeConstraint (dmn $ qE e) $ dmn $ convDT t
        (R.Error          _) -> Ident $ dmn "__BOMB")
    (\_  qB _ cE                  -> \case
        (R.E _ _ e)          -> qB e
        e                    -> Statements $ [dmn $ ExprStmt $ dmn $ cE e])

instance Show R.Expr where show = show . convRB



instance Show D.TypeError where
  show = \case
    D.TypeMismatch exp got -> "Expected " ++ show exp ++ ", got " ++ show got
    D.CantCaseAnalyze ty   -> "Cannot use \"Cases ... end\" to deconstruct " ++ show ty

instance Show D.Error where
  show = \case
    D.EndBlockWithDef    -> "The last element in a block must be an expression"
    D.SameLineStatements -> "Two statements should never be put on the same line"

instance Show R.Error where
  show = \case
    R.Earlier    error     -> show error

    R.UnboundId  ident     -> show ident ++ " is unbound"
    R.MutateVar  loc ident -> "cannot mutate non-variable " ++ ident ++ ", bound at " ++ show loc

    R.DupIdent dt loc iden -> sentance1 ++ " one of them is bound at " ++ show loc ++ "."
      where sentance1 = case dt of
              R.Pattern -> "pattern binds multiple identifiers named " ++ show iden ++ "."
              R.Constr  -> "type has multiple variants named "         ++ show iden ++ "."
              R.Graph   -> "graph has multiple declerations named "    ++ show iden ++ "."

    R.TypeError ty err -> show err ++ " in " ++ show ty

instance Show R.TypeError where
  show = \case
    R.TEEarlier terror    -> show terror

    R.AmbiguousType        -> "ambiguous type ecountered"
    R.PartialObj fields    -> "ambiguous object type encountered with fields " ++ show fields
