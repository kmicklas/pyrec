{-# LANGUAGE FlexibleInstances #-}
module Pyrec.PrettyPrint where

import           Prelude

import           Control.Applicative

import           Data.List
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

import           Pyrec.Error

class PrettyPrint a where
  pp :: a -> String

parenList :: [String] -> String
parenList l = "("  ++ intercalate ", " l ++ ")"

angleList :: [String] -> String
angleList l = "<"  ++ intercalate ", " l ++ ">"

curlyList :: [String] -> String
curlyList l = "{"  ++ intercalate ", " l ++ "}"

indentedLines :: [String] -> String
indentedLines []  = ": ;"
indentedLines [l] = ": " ++ l ++ ";"
indentedLines l   = ":\n" ++ (concat $ fmap ("\n  "++) l) ++ "end"

opt :: Maybe String -> String
opt Nothing  = ""
opt (Just i) = i

instance PrettyPrint n => PrettyPrint (Node n) where
  pp (Node _ n) = pp n

instance PrettyPrint String where
  pp s = s

instance PrettyPrint e => PrettyPrint (Message e) where
  pp (Msg loc error) = show loc ++ ":\n" ++ pp error

instance PrettyPrint id => PrettyPrint (Bind id) where
  pp (Bind id ty) = pp id ++ (opt $ (" :: " ++) <$> pp <$> ty)

instance PrettyPrint Statement where
  pp = \case
    (ExprStmt   e)   ->  pp e
    (LetStmt letd)   ->           plet letd
    (VarStmt letd)   -> "var " ++ plet letd
    (AssignStmt i e) -> pp i ++ " := " ++ pp e
    (Graph decls)    -> "graph" ++ pp decls

    (FunStmt tps id ps ret body) ->
      "fun"
      ++ (opt $ angleList <$> fmap pp <$> tps)
      ++ " " ++ pp id
      ++ (opt $ parenList <$> fmap pp <$> ps)
      ++ (opt $ (" -> " ++) . pp <$> ret) ++ pp body

    (Data id params variants) ->
      "data " ++ pp id
      ++ (opt $ angleList <$> fmap pp <$> params)
      ++ (indentedLines $ pp <$> variants)

instance PrettyPrint Block where
  pp (Statements stmts) = indentedLines $ fmap pp stmts

instance PrettyPrint Variant where
  pp (Variant id binds) = "|  " ++ pp id ++ (parenList $ pp <$> binds)

instance PrettyPrint Type where
  pp = \case
    (TIdent id)         -> pp $ id
    (TFun   params ret) -> (parenList $ pp <$> params) ++ " -> " ++ pp ret
    (TParam params ret) -> (angleList $ pp <$> params) ++           pp ret
    (TObject fields)    -> curlyList $ pp <$> fields

plet   :: Let Id  -> String
pfield :: Let Key -> String
(plet, pfield) = (pp' " = ", pp' " : ")
  where pp' token (Let bind expr) = pp bind ++ " = " ++ pp expr

instance PrettyPrint Expr where
  pp = \case
    (Num   d)   -> show d
    (Str   s)   -> show s
    (Ident i)   -> pp $ i

    (App  e es) -> (pp e ++) $ parenList $ pp <$> es
    (AppT e ts) -> (pp e ++) $ angleList $ pp <$> ts

    (UnOp  tok e)     -> tok ++ pp e
    (BinOp first rest) -> "(" ++ pp first
                          ++ concat ((\ (op, e) -> " " ++ op ++ " " ++ pp e)
                                       <$> rest)
                          ++ ")"

    (Fun tps ps retT body) -> "fun"
                              ++ (opt $ angleList <$> fmap pp <$> tps)
                              ++ (opt $ parenList <$> fmap pp <$> ps)
                              ++ (opt $ (" -> " ++) . pp <$> retT) ++ pp body

    (Block block)          -> "block" ++ pp block
    (TypeConstraint e t)   -> "(" ++ pp e ++ " :: " ++ pp t ++ ")"

    (Obj fields)           -> curlyList $ pp <$> fields

instance PrettyPrint Field where
  pp = \case
    Immut l -> pfield l
    Mut   l -> "mutable " ++ pfield l

instance PrettyPrint Key where
  pp = \case
    Name    id  -> pp id
    Dynamic exp -> "mutable " ++ pp exp

instance PrettyPrint Module where
  pp (Module provide imports block) = pp provide
                                        ++ "\n\n" ++ (unlines $ pp <$> imports)
                                        ++ "\n\n" ++ pp block

instance PrettyPrint Provide where  pp _ = ""
instance PrettyPrint Import where  pp _ = ""


dmn q = Node (newPos "derp-dummy" 0 0) q

mkFunctions ::
  forall bt bn id ty ex
  .  ((bt -> Bind Id)                                    -> (ty -> Type)                  -> bt -> Bind Id)
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
    (\cT qT _  _               -> TIdent . dmn . \case
        (D.T t)                           -> pp $ qT t
        (D.TUnknown)                     -> "?"
        (D.PartialObj fields)            -> "PARTIAL" ++ (pp $ TObject $ f <$> M.toList fields)
          where f (s,t) = Bind (dmn s) $ Just $ dmn $ cT t
        (D.TError (D.TypeMismatch a b))  -> "Mismatch(" ++ (pp $ cT a) ++ ", " ++ (pp $ cT b) ++ ")"
        (D.TError (D.CantCaseAnalyze t)) -> "Can'tCase(" ++ (pp $ cT t) ++ ")")
    (\cE qE cT _               -> \case
        (D.E _ D.TUnknown e) -> qE e
        (D.E _ t          e) -> TypeConstraint (dmn $ qE e) $ dmn $ cT t
        (D.Constraint _ t e) -> TypeConstraint (dmn $ cE e) $ dmn $ cT t)
    (\_  qB _ cE               -> \case
        (D.E _ _ e)          -> qB e
        e                    -> Statements $ [dmn $ ExprStmt $ dmn $ cE e])

instance PrettyPrint D.Expr where pp = pp . convDB
instance PrettyPrint D.Type where pp = pp . convDT

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
        (C.E _ _ e)          -> qB e)

instance PrettyPrint C.Expr where pp = pp . convCB

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

instance PrettyPrint R.Expr where pp = pp . convRB



instance PrettyPrint D.TypeError where
  pp = \case
    D.TypeMismatch exp got -> "Expected " ++ pp exp ++ ", got " ++ pp got
    D.CantCaseAnalyze ty   -> "Cannot use \"Cases ... end\" to deconstruct " ++ pp ty

instance PrettyPrint D.Error where
  pp = \case
    D.EndBlockWithDef    -> "The last element in a block must be an expression"
    D.SameLineStatements -> "Two statements should never be put on the same line"

instance PrettyPrint R.Error where
  pp = \case
    R.Earlier    error     -> pp error

    R.UnboundId  ident     -> show ident ++ " is unbound"
    R.MutateVar  loc ident -> "cannot mutate non-variable " ++ ident ++ ", bound at " ++ show loc

    R.DupIdent dt loc iden -> sentance1 ++ " one of them is bound at " ++ show loc ++ "."
      where sentance1 = case dt of
              R.Pattern -> "pattern binds multiple identifiers named " ++ iden ++ "."
              R.Constr  -> "type has multiple variants named "         ++ iden ++ "."
              R.Graph   -> "graph has multiple declerations named "    ++ iden ++ "."

    R.TypeError ty err -> pp err ++ " in " ++ pp ty

instance PrettyPrint R.TypeError where
  pp = \case
    R.TEEarlier terror    -> pp terror

    R.AmbiguousType        -> "ambiguous type ecountered"
    R.PartialObj fields    -> "ambiguous object type encountered with fields "
                              ++ (curlyList $ (++ ["..."]) $ for (M.toList fields) $
                                \ (f, t) -> f ++ ": " ++ pp t)
