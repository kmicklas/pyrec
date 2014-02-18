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

  Fun bds e -> bad rs $ Fun bds e'
    where (rs, e') = report e

  Let d e -> bad (rs1 ++ rs2) $ Let d' e'
    where (rs1, d') = decl d
          (rs2, e') = report e

  Graph ds e -> bad (concat rs1 ++ rs2) $ Graph ds' e'
    where (rs1, ds') = unzip $ map decl ds
          (rs2, e')  = report e

  App f as -> bad (rs1 ++ concat rs2) $ App f' as'
    where (rs1, f')  = report f
          (rs2, as') = unzip $ map report as

  Try e1 bd e2 -> bad (rs1 ++ rs2) $ Try e1' bd e2'
    where (rs1, e1') = report e1
          (rs2, e2') = report e2

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
        bad rs e = (rs, oe e)
        good e = bad [] e


decl :: Decl Bind C.Id C.Expr -> (Errors, Decl Bind R.Id R.Expr)
decl (Def k b o) = (rs, Def k b o')
  where (rs, o') = report o
