{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
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

instance Show Bind where
  show (Bind id ty) = show id ++ opt (show <$> ty)

sblock :: [Node Statement] -> String
sblock = lines . fmap show

instance Show Statement where
  show s = case s of
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

instance Show Variant where
  show (Variant id binds) = "|  " ++ show id ++ (parenList $ show <$> binds)

instance Show Type where
  show t = case t of
    (TIdent id)         -> show id
    (TFun   params ret) -> (parenList $ show <$> params) ++ show ret
    (TParam params ret) -> (angleList $ show <$> params) ++ show ret
    (TObject fields)    -> curlyList $ show <$> fields

instance Show Let  where
  show (Let bind expr) = show bind ++ " = " ++ show expr

instance Show Expr where
  show e = case e of
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

mkFunctions :: forall bt bn id ty ex
            .  (bt -> Bind) -> (bn -> Id) -> (id -> Id)
            -> ((IR.Type bn id ty       -> Type)  -> ty -> Type)
            -> ((IR.Expr bt bn id ty ex -> Expr)  -> ex -> Expr)
            -> ((IR.Expr bt bn id ty ex -> Block) -> ex -> Block)
            -> ( IR.Expr bt bn id ty ex -> Expr
               , IR.Expr bt bn id ty ex -> Block
               , IR.Type bn id ty       -> Type)
mkFunctions bt bn id ty ex bl = (resugarE, resugarB, resugarT)
  where
    dmn q = Node (newPos "derp-dummy" 0 0) q
    ex' = ex resugarE
    bl' = bl resugarB
    ty' = ty resugarT

    resugarE :: IR.Expr bt bn id ty ex -> Expr
    resugarE e = case e of
      (IR.Num n)     -> Num n
      (IR.Str s)     -> Str s
      (IR.Ident i)   -> Ident $ id i
      (IR.Fun  bs e) -> Fun Nothing            (Just $ bt <$> bs) Nothing $ bl' e
      (IR.FunT bs e) -> Fun (Just $ bn <$> bs) Nothing            Nothing $ bl' e

      (IR.App  f args) -> App  (dmn $ ex' f) $ dmn <$> ex' <$> args
      (IR.AppT f args) -> AppT (dmn $ ex' f) $ dmn <$> ty' <$> args

      (IR.Cases _ _ _) -> undefined

      (IR.Try _ _ _)      -> undefined
      (IR.EmptyObject)    -> undefined
      (IR.Extend obj f v) -> undefined
      (IR.Access obj f)   -> undefined

      (IR.Let   _ _)   -> Block $ resugarB e
      (IR.Graph _ _)   -> Block $ resugarB e
      (IR.Assign _ _)  -> Block $ resugarB e

    resugarB :: IR.Expr bt bn id ty ex -> Block
    resugarB e = case e of
      (IR.Let d r)       -> convDecl d                         : bl' r
      (IR.Graph decls r) -> (dmn $ Graph $ convDecl <$> decls) : bl' r
      (IR.Assign i e)    -> [dmn $ AssignStmt (id i) $ dmn $ ex' e]

      _                  -> [dmn $ ExprStmt $ dmn $ resugarE e]

    convDecl :: IR.Decl bt bn ex -> Node Statement
    convDecl d = dmn $ case d of
      (IR.Data b vs)   -> Data (bn b) Nothing $ variants <$> vs
      (IR.Def  dt b v) -> k $ Let (bt b) (dmn $ ex' v)
        where k = case dt of IR.Val -> LetStmt
                             IR.Var -> VarStmt

    variants :: IR.Variant bt bn -> Variant
    variants (IR.Variant b ps) = Variant (bn b) $ bt <$> fromMaybe [] ps

    resugarT :: IR.Type bn id ty -> Type
    resugarT t = case t of
      (IR.TIdent i)          -> TIdent $ id i
      (IR.TFun   types  ret) -> TFun   (ty' <$> types)  $ ty' ret
      (IR.TParam params ret) -> TParam (bn  <$> params) $ ty' ret

      (IR.TType)             -> TIdent $ dmn "__TYPE"

      (IR.TObject fieldMap)  -> TObject $ for (M.toList fieldMap) $ \(i,ty) ->
        Bind (dmn i) $ Just $ dmn $ ty' ty

    --type resugE = IR.Expr bt bn id ty ex -> Expr
    --type resugB = IR.Expr bt bn id ty ex -> Block

{-
instance (Show bt, Show bn, Show id, Show ty, Show ex) => Show (Type bn id ty) where
  show e = case e of

    (Ident id)   -> show

instance (Show bn, Show id, Show ty) => Show (Type bn id ty) where
  show t = case t of
    (TIdent id)       -> show id
    (TFun params r)   -> "(" ++ intercalate ", " (show <$> params) ++ ")" ++
                         " -> " ++ show r
    (TParam params r) -> "<" ++ intercalate ", " (show <$> params) ++ ">" ++
                         " " ++ show r
    TType             -> "Type"

instance Show Type where
  show t = case t of
    (T t)               -> show t
    TUnknown            -> "?"
    (PartialObj fields) -> "{" ++ (intercalate ", " $ map f $ M.toList fields) ++ "}"
      where f (k,v) = show k ++ " : " ++ show v
    (TError e)          -> show e

instance Show TypeError where
  show e = case e of
    TypeMismatch exp got -> "Expected " ++ show exp ++ ", got " ++ show got
    CantCaseAnalyze ty   -> "Cannot use \"Cases ... end\" to deconstruct " ++ show ty

instance Show Error where
  show e = case e of
    EndBlockWithDef    -> "The last element in a block must be an expression"
    SameLineStatements -> "Two statements should never be put on the same line"

-}
