module Pyrec.Compile where

import qualified Data.Map          as M
import           Data.Map               (Map)

import           Pyrec.Misc

import qualified Pyrec.IR         as IR
import qualified Pyrec.IR.Core    as R
import qualified Pyrec.IR.Parse   as R (BindN(BN), Loc)
import qualified Pyrec.IR.Desugar as D (BindT(BT))
import           Pyrec.SSA

type Env = Map Id (IR.Decl () Id ())
type Chunk = (Module, [Statement], Id)

tempId :: R.Loc -> Id
tempId l = "%temp$" ++ show l

constId :: R.Loc -> Id
constId l = "@const$" ++ show l

bound :: R.Id -> Id
bound (R.Bound l n) = "%" ++ n ++ "$" ++ show l

caseBN :: R.BindN -> Id
caseBN (R.BN l n) = "@case$" ++ n ++ "$" ++ show l

ssa :: Env -> R.Expr -> Chunk
ssa env (R.E l _ e) = case e of

  IR.Num n -> (M.singleton cid (Num n), [Bind id $ Atomic $ Const cid], id)
    where cid = constId l
          id  = tempId  l

  IR.Ident id -> case M.lookup (bound id) env of
    Just (IR.Def IR.Val () ()) -> (M.empty, [], bound id)
    Just (IR.Def IR.Var () ()) -> (M.empty, [Bind valId $ Load $ bound id], valId)
      where valId = tempId l
    Just (IR.Data _ _) -> error "Can't use type as value"
    Nothing    -> (M.empty, [], ffi id) -- For FFI
      where ffi (R.Bound _ n) = n

  IR.Let (IR.Def IR.Val (D.BT bl n _) v) subE ->
    combine (vm, vb ++ [Bind id $ Atomic $ Bound vid], id)
            (sm, sb, sid)
    where (vm, vb, vid) = ssa env v
          (sm, sb, sid) = ssa (M.insert (bound bind) (IR.Def IR.Val () ()) env)
                              subE
          bind = R.Bound bl n
          id = bound bind

  IR.Let (IR.Def IR.Var (D.BT bl n _) v) subE ->
    combine (vm, vb ++ [Bind id Alloca, Assign id $ Bound vid], id)
            (sm, sb, sid)
    where (vm, vb, vid) = ssa env v
          (sm, sb, sid) = ssa (M.insert (bound bind) (IR.Def IR.Var () ()) env)
                          subE
          bind = R.Bound bl n
          id = bound bind

  IR.Assign id v -> (vm, vb ++ [Assign (bound id) $ Bound vid], vid)
    where (vm, vb, vid) = ssa env v

  IR.App f args -> foldr combine
                        (fm, fb ++ [Bind valId $ Call (Bound fid) argsIds], valId)
                        argsSSA
    where (fm, fb, fid) = ssa env f
          argsSSA       = map (ssa env) args
          argsIds       = for argsSSA $ \ (_, _, id) -> Bound id
          valId         = tempId l

  IR.Let (IR.Data id variants) subE -> (M.insert elim (Eliminator cases) sm, sb, sid)
    where (sm, sb, sid) = ssa undefined subE
          elim          = constId l
          cases         = for variants $ \ (IR.Variant vid binds) ->
            (caseBN vid, fmap length binds)

combine :: Chunk -> Chunk -> Chunk
combine (am, ab, _) (bm, bb, id) = (M.union am bm, ab ++ bb, id)

toModule :: Chunk -> Module
toModule (m, bs, id) =
  M.insert "@pyret_main" (Fun [] [Block bs $ Return $ Bound id]) m

compile :: R.Expr -> Module
compile = toModule . ssa M.empty
