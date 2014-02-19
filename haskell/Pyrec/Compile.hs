module Pyrec.Compile where

import qualified Data.Map as M
import           Data.Map (Map)

import qualified Pyrec.AST as A
import qualified Pyrec.AST.Core as R
import qualified Pyrec.AST.Parse as R (Bind(B), Loc)
import           Pyrec.SSA

type Env = Map Id A.DefType
type Chunk = (Module, [Bind], Id)

tempId :: R.Loc -> Id
tempId l = "%temp$" ++ show l

constId :: R.Loc -> Id
constId l = "@const$" ++ show l

bound :: R.Id -> Id
bound (R.Bound l n) = "%" ++ n ++ "$" ++ show l

ssa :: Env -> R.Expr -> Chunk
ssa env (R.E l _ e) = case e of

  A.Num n -> (M.singleton cid (Num n), [Bind id $ Atomic $ Const cid], id)
    where cid = constId l
          id  = tempId  l

  A.Ident id -> case M.lookup (bound id) env of
    Just A.Val -> (M.empty, [], bound id)
    Just A.Var -> (M.empty, [Bind valId $ Load $ bound id], valId)
      where valId = tempId l

  A.Let (A.Def A.Val (R.B bl n _) v) subE ->
    combine (vm, vb ++ [Bind id $ Atomic $ Bound vid], id)
            (sm, sb, sid)
    where (vm, vb, vid) = ssa env v
          (sm, sb, sid) = ssa (M.insert (bound bind) A.Val env) subE
          bind = R.Bound bl n
          id = bound bind

combine :: Chunk -> Chunk -> Chunk
combine (am, ab, _) (bm, bb, id) = (M.union am bm, ab ++ bb, id)

toModule :: Chunk -> Module
toModule (m, bs, id) = M.insert "@pyret_main" (Fun [] [Block bs $ Return $ Bound id]) m

compile :: R.Expr -> Module
compile = toModule . ssa M.empty
