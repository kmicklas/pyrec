#lang pyret

import ast as A
import file as F

Loc = A.Loc

data Id:
  | id(l :: Loc, n :: String)
  | gen(n :: String)
end

data LExpr:
  | at(l :: Loc, t :: Expr<LExpr>, e :: Expr<LExpr>)
end

data TExpr:
  | typed(t :: Expr<TExpr>, e :: Expr<TExpr>)
end

data Expr<E>:
  | e_let(d :: Decl<E>, in :: E)
  | e_graph(ds :: List<Decl<E>>, in :: E)
  | e_assign(id :: Id, v :: E)
  | e_cases(e :: E, t :: E, branches :: List<Case<E>>)
  | e_try(body :: E, _except :: E) # except is lambda
  | e_fun(args :: List<Id>, body :: E)
  | e_app(fn :: E, args :: List<E>)
  | e_id(id :: Id)
  | e_num(n :: Number)
  | e_str(s :: String)
  | t_type
  | t_unknown
  | t_any
  | t_num
  | t_str
  | t_fun(params :: List<Id>, args :: List<Arg>, result :: E)
end

data Decl<E>:
  | d_val(b :: Bind<E>, v :: E)
  | d_var(b :: Bind<E>, v :: E)
  | d_data(b :: Bind<E>, params :: List<String>, variants :: List<Variant>)
end

data Bind<E>:
  | Bind(n :: String, t :: E)
end

data Case<E>:
  | case(p :: Pattern<E>, e :: E)
end

data Pattern<E>:
  | constr(n :: String, args :: Option<List<Pattern<E>>>)
  | bind(b :: Bind<E>)
end

data Variant<E>:
  | variant(n :: String, args :: List<Arg<E>>)
end

################################################################################

fun new-id(): gen(gensym("%")) end

fun desugar-bind(b :: A.Bind) -> Bind<LExpr>:
  
end

fun desugar(expr :: A.Expr) -> LExpr:
  fun mk(l, e): at(l, t_unknown, e) end
  
  cases(A.Expr) expr:
    | s_block(l, stmts) =>
      cases(List<A.Expr>) stmts.reverse():
        | empty => mk(l, e_id("nothing"))
        | link(last, rest) =>
          for fold(b from desugar(last), e from rest):
            cases(A.Expr) e:
              | s_let(ll, n, v) =>
                mk(ll, e_let(b_val(n.id, desugar(v)), b))
              | s_var(ll, n, v) =>
                mk(ll, e_let(b_var(n.id, desugar(v)), b))
              | s_graph(ll, bindings) => raise("Don't know how to graph yet")
              | else => e_let(b_val(new-id(), desugar(e)), b)
            end
          end
      end
    | s_user_block(l, b) => mk(l, desugar(b))
    | s_let(_, _, _) => raise("Didn't expect let out of block")
    | s_var(_, _, _) => raise("Didn't expect var out of block")
    | s_assign(l, n, v) => mk(l, e_assign(n, desugar(v)))
    | s_op(l, op, left, right) =>
      fn = if      op == "op+": "%add"
           else if op == "op-": "%sub"
           else if op == "op*": "%mul"
           else if op == "op/": "%div"
           else: raise("Unrecognized operator " + op)
           end
      mk(l, e_app(mk(l, e_id(fn)), map(desugar, [left, right])))
    | s_id(l, n) => mk(l, e_id(n))
    | s_num(l, n) => at(l, t_num, e_num(n))
    | else => raise("Don't know how to handle this")
  end
end

fun parse(file :: String) -> A.Program:
  A.parse(F.input-file(file).read-file(),
          file,
          {check : false}).pre-desugar
end

desugar(parse("test.arr").block)
