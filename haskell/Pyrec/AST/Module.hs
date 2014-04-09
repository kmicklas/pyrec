module Pyrec.AST.Module where
-- The AST for multi-module programs

data Module ex = Module (Provide ex) [Import] ex

data Provide ex
  = NoProvide
  | ProvideAll
  | ProvideExpr ex

data Import
  = Import ModuleName
  | ImportQualified ModuleName String

data ModuleName
  = Named String
  | File String
