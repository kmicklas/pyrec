module Pyrec.Report where

import           Pyrec.AST
import           Pyrec.AST.Parse (Bind)
import           Pyrec.AST.Check as C
import qualified Pyrec.AST.Core  as R

type Errors = [R.ErrorMessage]

report :: C.Expr -> (Errors, R.Expr)
report (E l t e) = case e of

  Num n -> good $ Num n
  Str s -> good $ Str s

  Fun bds e -> (,) rs $ oe $ Fun bds e'
    where (rs, e') = report e

  Let d e -> (,) (rs1 ++ rs2) $ oe $ Let d' e'
    where (rs1, d') = decl d
          (rs2, e') = report e

  Graph ds e -> (rs ++ (concat $ map fst temp), e')
    where temp = map decl ds
          (rs, e') = report e

  Ident i -> case i of
    Bound      il is -> stuff il is
    NotMutable il is -> stuff il is
    Unbound       is -> helper $ R.UnboundId is
    where helper err = ([err'], R.Error err')
            where err' = (l, err)
          stuff a b = good $ Ident $ R.Bound a b

  Assign i v -> case i of
    Bound il is -> (,) rs $ oe $ Assign (R.Bound il is) v'
    NotMutable il is -> helper $ R.MutateVar il is
    Unbound       is -> helper $ R.UnboundId is
    where (rs, v') = report v
          helper err = (err' : rs, R.Error err')
            where err' = (l, err)

  where oe e = R.E l t e
        good e = ([], oe e)

decl :: Decl Bind C.Id C.Expr -> (Errors, Decl Bind R.Id R.Expr)
decl (Def k b o) = (rs, Def k b o')
  where (rs, o') = report o
