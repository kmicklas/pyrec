{-# LANGUAGE EmptyCase #-}

module Pyrec.ScopeCheck where

import           Prelude                 hiding (mapM, sequence)

import           Control.Applicative
import           Control.Monad.Reader    hiding (mapM, sequence)

import           Data.List                      (foldl', find)
import qualified Data.Map          as M
import           Data.Map                       (Map)
import           Data.Maybe                     (fromMaybe)
import           Data.Traversable        hiding (for)

import           Pyrec.Misc

import qualified Pyrec.IR          as IR        (Pattern)
import           Pyrec.IR                hiding (Pattern)
import           Pyrec.IR.Desugar  as D
import qualified Pyrec.IR.Check    as C

type Pattern = IR.Pattern D.BindT BindN
data Entry   = Entry Unique DefType
type Env     = Map Id Entry
type CK      = Reader Env

extendMap :: Map Id a -> Map Id a -> Map Id a
extendMap = flip M.union

emptyEnv :: Env
emptyEnv = M.empty

bindNParam :: BindN -> (Id, Entry)
bindNParam (BN l i)   = (i, Entry l Val)

bindTParam :: BindT -> (Id, Entry)
bindTParam (BT l i _) = (i, Entry l Val)

bindT :: Env -> BindT -> C.BindT
bindT env (BT l i t) = C.BT l i $ scT env t

newId env i = case M.lookup i env of
  Nothing           -> C.Unbound    i
  Just (Entry l dt) -> C.Bound dt l i

scT :: Env -> D.Type -> C.Type
scT _   TUnknown = C.TUnknown
scT env (D.T t)  = C.T $ case t of
  TType             -> TType  
  TIdent i          -> TIdent $ newId env i
  TFun   params ret -> TFun   (rT <$> params) $ rT ret
  TParam params ret -> TParam params          $ scT env' ret
    where env' = extendMap env $ M.fromList $ bindNParam <$> params
  TObject fields    -> TObject $ rT <$> fields

  where rT = scT env

scE :: Env -> D.Expr -> C.Expr
scE env (D.Constraint l t e) = C.Constraint l (scT env t) (scE env e)
scE env (D.E          l t e) = C.E l (rT t) $ case e of

  Num n -> Num n
  Str s -> Str s

  Fun params body -> Fun (bindT env <$> params) $ scE env' body
    where env' = extendMap env $ M.fromList $ bindTParam <$> params

  FunT params body -> FunT params $ scE env' body
    where env' = extendMap env $ M.fromList $ bindNParam <$> params

  Let decl e -> Let decl' $ scE env' e
    where (decl', env') = case decl of

            (Def kind (BT vl vi _) v) ->
              (Def kind (C.BT vl vi $ rT t) $ scE env' v, env')
              where env' = M.insert vi (Entry vl kind) env

            (Data b vs) -> (Data b vs', env'')
              where env'  = uncurry M.insert (bindNParam b) env
                    vs'   = for vs $ \(Variant constr fs) ->
                      Variant constr $ bindT env' <$$> fs
                    env'' = extendMap env' $ M.fromList $ bindNParam 
                            <$> (\(Variant c _) -> c) <$> vs

  App  f args -> App  (rE f) (rE <$> args)
  AppT f args -> AppT (rE f) (rT <$> args)

  Ident  i   -> Ident              $ newId env i
  Assign i v -> flip Assign (rE v) $ newId env i

  Cases vt v cases -> Cases (rT vt) (rE v) $ c <$> cases
    where c (Case p e) = Case (pat' p) $ scE (M.union env $ accum p) e
          pat' = \case
            (Binding (BT l i t)) -> Binding $ C.BT l i $ rT t
            (Constr c pats)      -> Constr c $ pat' <$$> pats
          accum = \case
            (Binding (BT l i _)) -> M.singleton i $ Entry l Val
            (Constr _ pats)      -> fromMaybe M.empty $ M.unions <$> accum <$$> pats

  EmptyObject      -> EmptyObject
  Extend obj fi fv -> Extend (rE obj) fi (rE fv)
  Access obj fi    -> Access (rE obj) fi

  where rE = scE env
        rT = scT env
