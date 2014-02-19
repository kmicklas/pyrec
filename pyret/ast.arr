#lang pyret

# Base AST

data Expr<bd, id, ex, ty>:
  | num    (d    :: Number)
  | str    (s    :: String)
  | ident  (i    :: id)
  | lam    (bds  :: List<bd>,           body :: ex)
  | let    (bd   :: Decl<bd,id,ex>,       in :: ex)
  | graph  (bds  :: List<Decl<bd,id,ex>>, in :: ex)
  | app    (f    :: ex, args :: List<ex>)
  | assign (id   ::  id, new :: ex)
#   | case   (type :: ty, of  :: ex, pats :: List<Case<bd, id, ex>>)
  | try    (try  :: ex, bd :: bd, catch :: ex)
end

data Type<id, ty>:
  | TAny
  | TNum
  | TStr
  | TIdent (id :: id)
  | TFun   (params :: List<ty>, ret :: ty)
end

data DefType:
  | Val
  | Var
end
 
data Decl<bd,id,ex>:
  | Def  (kind :: DefType, bd :: bd, val :: ex)
#   | Data (id List<Variant bd id>
end
 
# data Cases<bd, id, ex>:
#   | case(pattern :: Pattern<bd, id>, ex :: ex)
# end
 
# data Pattern bd id
#   = Constr id (Maybe List<Pattern bd id>)
#   | Binding bd
# end
 
# data Variant bd id
#   = Variant id (Maybe List<bd>)
# end
