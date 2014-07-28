{-# LANGUAGE StandaloneDeriving #-}

module Pyrec.TypeCheck where

import           Prelude                   hiding (mapM, sequence)

import           Control.Applicative
import           Control.Monad             hiding (mapM, sequence)
import           Control.Monad.Reader      hiding (mapM, sequence)

import           Data.List                        (foldl', find)
import qualified Data.Map            as M
import           Data.Map                         (Map)
import           Data.Maybe                       (fromMaybe)
import           Data.Traversable          hiding (for)

import           Pyrec.Misc
import           Pyrec.Error

import qualified Pyrec.IR            as IR        (Pattern)
import           Pyrec.IR                  hiding (Pattern)
import qualified Pyrec.IR.Desugar    as D
import           Pyrec.IR.Desugar                 (BindN(..))
import           Pyrec.IR.ScopeCheck as SC
import qualified Pyrec.IR.TypeCheck  as TC
import           Pyrec.IR.TypeCheck               (CType, RType)

deriving instance Ord BindN

type Pattern = IR.Pattern RType BindN
type Entry   = Decl BindT BindN ()
type Env     = Map BindN Entry

type TC      = Maybe

extendMap :: Map BindN a -> Map BindN a -> Map BindN a
extendMap = flip M.union

bindNParam :: BindN -> (BindN, Entry)
bindNParam k@(BN l i) = (k, Def Val (BT l i $ SC.T TType) ())

bindTParam :: BindT -> (BindN, Entry)
bindTParam (BT l i t) = (BN l i, Def Val (BT l i t) ())

extendT :: BindT -> Map BindN Entry -> Map BindN Entry
extendT = uncurry M.insert . bindTParam

-- refines a constraint type
refineC :: Env -> CType -> CType -> TC CType
refineC = _

-- refines a result type
refineR :: Env -> CType -> RType -> TC RType
refineR = _

mkCT = SC.T
mkRT = TC.T

constrain :: Env -> SC.Expr -> CType -> TC TC.Expr
constrain env e ot = typeCheckH env e =<< refineC env ot it
  where it = case e of
          (SC.Constraint _ it _) -> it
          (SC.E          _ it _) -> it

synth :: Env -> SC.Expr -> TC TC.Expr
synth env e = typeCheckH env e t
  where t = case e of
          (SC.Constraint _ t _) -> t
          (SC.E          _ t _) -> t

-- big helper function, recur with constrain or synth
typeCheckH :: Env -> SC.Expr -> CType -> TC TC.Expr
typeCheckH env (SC.Constraint _ _ e) t = constrain env e t
typeCheckH env (SC.E          l _ e) t = _ {- case e of

  -- Need too look up to get proper loc
  Num n -> se (refineR env t $ SC.T $ TIdent $ Bound Val Intrinsic "Number") $ Num n
  Str s -> se (refineR env t $ SC.T $ TIdent $ Bound Val Intrinsic "String") $ Str s

  Ident (Unbound     _) -> se TUnknown e
  Ident (Bound _ il ii) -> k e
    where k = case M.lookup (BN il ii) env of
            Nothing                    -> error "internal: identifier was reported bound"
            Just (Def _ (BT _ _ t') _) -> se $ refineR env t t'
            Just (Data  _           _) -> se $ refineR env t $ mkCT TType

  Let (Def kind b@(BT vl vi vt) v) e -> SC.E l t' $ Let newDef e'
    where v'@(TC.E _ vt' _) = constrain env v vt
          newDef            = Def kind (BT vl vi vt') v'
          env'              = extendT b env
          e'@(TC.E _ t'  _) = constrain env' e t

  Let (Data i@(BN _ di) variants) e -> se t' $ Let data' e'
    where fixVariant :: Variant BindT BindN -> Variant BindT BindN
          fixVariant (Variant vi args) = Variant vi $ fixField <$$> args
            where fixField :: BindT -> BindT
                  fixField (BT bl bi t) = BT bl bi $ checkT env1 t

          bindConstrs :: Variant D.BindT BindN -> (BindN , Entry)
          bindConstrs (Variant b@(BN vl vi) ps) =
            (b, Def Val (BT vl vi $ mkRT $ k $ TIdent $ Bound Val l di) ())
            where k = case ps of
                    Nothing     -> id
                    Just params -> TFun (for params $ \(TC.BT _ _ t) -> t) . mkRT

          envData         :: Entry -- phantom ex type parameter nessesitates rebuild
          envData           = Data i variants
          -- | env + type, without constructors
          env1              = M.insert i envData env
          env2              = extendMap env1 $ M.fromList $ (i, envData) : fmap bindConstrs variants'
          variants'         = fixVariant <$> variants
          e'@(TC.E _ t'  _) = constrain env2 e t
          data'             = Data i variants'

  Assign i v -> se t'' $ Assign i v' -- immutability errors caught downstream in Pyrec.Report
    where t' = case i of
            Unbound _     -> t
            Bound _ al ai -> case M.lookup (BN al ai) env of
              Nothing                    -> error "internal: identifier was reported bound"
              Just (Def _ (BT _ _ t') _) -> t'
              Just (Data  _           _) -> mkRT TType

          v'@(TC.E _ t'' _) = constrain env v t'

  Fun params body -> se t' $ Fun params' body'
    where params'               = for params $ \(BT l i t) -> BT l i $ checkT env t
          env'                  = extendMap env $ M.fromList $ bindTParam <$> params'
          body'@(TC.E _ retT _) = synth env' body
          t'                    = unify env t $ SC.T
                                  $ TFun (for params' $ \(BT _ _ pt) -> pt) retT

  FunT params body -> se t' $ FunT params body'
    where env'                  = extendMap env $ M.fromList $ bindNParam <$> params
          body'@(TC.E _ retT _) = synth env' body
          t'                    = unify env t $ SC.T $ TParam params retT

  App f args -> se t' $ App f' args'
    where f'@(TC.E _ ft' _)     = synth env f
          (t' , args')          = case ft' of
             TC.T (TFun pts retT) -> (k, args')
               where k     = se $ refineR t retT
                     args' = case map2S (constrain env) pts args of
                       Just args'  -> args'
                       Nothing     -> _
             _                    -> _ -- SC.TError $ SC.TypeMismatch ft ft'

  AppT f args -> se t' $ AppT f' args
    where f'@(TC.E fl ft fe)   = synth env f
          t'                   =

          expected             = SC.T $ TParam t $ for [1..length args]
                                 $ \n -> BN fl $ 'T' : show n
          (t', ft')            = case ft of
            SC.T (TParam params retT) ->
              (,ft) $ case map2S (,) params (checkT env <$> args) of
                Just substs -> unify env t $ subst (M.fromList substs) retT
                Nothing     -> TError $ TypeMismatch t retT
            _                        -> (ft, TError $ TypeMismatch expected ft)
          f'                   = SC.E fl ft' fe

  Cases vt v cases -> se t'' $ Cases vt' v' cases'
    where v'@(C.E _ vt' _) = constrain env v vt

          bind :: SC.Type -> Pattern -> (Bool , Pattern , Env)
          bind t p = case p of

            (Binding b@(BT l i t')) -> (True, Binding $ b', extendT b' M.empty )
              where t'' = unify env t t'
                    b'  = BT l i t''

            (Constr ci pats) -> result
              where ensureIdent (T (TIdent (Bound Val l i))) = Just $ BN l i
                    ensureIdent _                            = Nothing

                    -- outer maybe signifies error, inner maybe is use to distinguish
                    -- pyret's "| Constructor()" vs "| Constructor"
                    params :: Maybe (Maybe [C.Type])
                    params = do (Variant _ params) <-
                                  ensureIdent t
                                  >>= (\i -> M.lookup i env)
                                  >>= (\x -> case x of
                                          Data _ vs -> Just vs
                                          _         -> Nothing)

                                  >>= find (\(Variant vi _) -> vi == ci)
                                guard $ (length <$> params) == (length <$> pats)
                                return $ flip (<$$>) params $ \(BT _ _ t) -> t

                    results :: Maybe [(Bool, Pattern , Env)]
                    (newerr, results) = case params of
                      Nothing     -> (False , bind TUnknown <$$> pats)
                      Just params -> (True  , zipWith bind <$> params <*> pats)

                    result = case results of
                      Nothing   -> ( newerr
                                   , Constr ci Nothing
                                   , M.empty)
                      Just list -> ( and $ newerr : errs
                                   , Constr ci $ Just pats
                                   , M.unions envs)
                        where (errs, pats, envs) = unzip3 list

          checkCase :: (Bool, SC.Type)  -> Case SC.BindT BindN SC.Expr ->
                       ((Bool , SC.Type) , Case SC.BindT BindN SC.Expr)
          checkCase (err, t) (Case pat body) = ((err && err' , t') , Case pat' body')
            where (err' , pat' , caseEnv) = bind vt' pat
                  body'@(C.E _ t'  _)     = constrain caseEnv body t

          ((err , t') , cases') = mapAccumL checkCase (True, t) cases

          t'' = if err then TError $ SC.CantCaseAnalyze t' else t'

  EmptyObject -> se (unify env t $ SC.T $ TObject M.empty) EmptyObject

  -- can this be made less ridiculous?
  Extend obj fi fv -> se t' $ Extend obj' fi fv'
    where fv' @(C.E _ ft  _) = synth env fv

          ot                 = case t of
            SC.T (TObject o)   -> SC.T $ TObject $ trim o
            SC.PartialObj p    -> SC.PartialObj  $ trim p
            _                 -> SC.PartialObj  $ M.empty
          trim map           = M.delete fi map

          obj'@(C.E _ ot' _) = constrain env obj ot

          t'                 = unify env t $ case ot' of
            SC.T (TObject o)   -> SC.T $ TObject $ tryRefine o
            SC.PartialObj p    -> SC.PartialObj  $ tryRefine p
            _                 -> SC.PartialObj  $ M.singleton fi ft
          tryRefine map      = M.insert fi ft map

  Access obj fi -> se t'' $ Access obj' fi
    where t'                 = SC.PartialObj $ M.singleton fi t
          obj'@(C.E _ ot  _) = constrain env obj t'
          t''                = unify env t $ case ot of
            SC.T (TObject o)   -> tryRefine o
            SC.PartialObj p    -> tryRefine p
            _                 -> t'
          tryRefine map      = fromMaybe t' $ M.lookup fi map

  where se = SC.E l


subst :: Map BindN SC.Type -> SC.Type -> SC.Type
subst substs = \case
  t@(C.T (TIdent (Bound Val l i))) -> fromMaybe t $ M.lookup (BN l i) substs
  t@(C.T (TIdent (Unbound _)))     -> t
  SC.T (TParam params retT)         -> SC.T $ TParam params $ recur retT
  SC.T inner                        -> SC.T        $ recur <$> inner
  PartialObj fields                -> PartialObj $ recur <$> fields
  t@(TError _)                     -> t
  t@(TUnknown)                     -> t -- redundancy to future-proof
  where recur = subst substs

checkT :: Env -> SC.Type -> SC.Type
checkT env = \case
  t@(C.T (TIdent (Bound Val l i))) -> case M.lookup (BN l i) env of
    Just (Def Val (BT _ _ (C.T TType)) _) -> t
    Just (Data    _                    _) -> t
    _                                     -> error "unbound, or non-type identifier"
  t@(C.T (TIdent (Unbound _)))     -> t
  SC.T (TParam params retT)         ->
    flip checkT retT $ extendMap env $ M.fromList $ bindNParam <$> params
  SC.T inner                        -> SC.T        $ checkT env <$> inner
  PartialObj fields                -> PartialObj $ checkT env <$> fields
  t@(TError _)                     -> t
  t@(TUnknown)                     -> t -- redundancy to future-proof

unify :: Env -> SC.Type -> SC.Type -> SC.Type
unify env t1 t2 = unifyWithSubsts env M.empty t1 t2

-- expected then got
unifyWithSubsts :: Env -> Map BindN BindN -> SC.Type -> SC.Type -> SC.Type
unifyWithSubsts env substs _a _b = case (_a, _b) of
  (C.T TType, SC.T TType) -> SC.T TType

  (C.T (TIdent i@(Bound Val al ai)), SC.T (TIdent (Bound Val bl bi))) ->
    try $ SC.T <$> do
      -- either a substitutes for b, or a has no substitution and a == b
      guard $ case M.lookup a substs of
        Just a' -> a' == b
        Nothing -> a  == b
      return $ TIdent i
    where a = (BN al ai)
          b = (BN bl bi)

  (C.T (TFun aParams aRes), SC.T (TFun bParams bRes)) ->
    try $ SC.T <$> do
      params <- map2S recur aParams bParams
      return $ TFun params $ recur aRes bRes

  (C.T (TParam aParams aRes), SC.T (TParam bParams bRes)) ->
    try $ SC.T <$> do
      news <- map2S (,) aParams bParams
      let substs' = extendMap substs $ M.fromList news
      return $ TParam aParams $ unifyWithSubsts env substs' aRes bRes

  ((C.T (TObject o1)), (C.T (TObject o2))) ->
    try $ SC.T <$> TObject <$> do
      guard $ M.size o1 == M.size o2             -- same size
      let match = M.intersectionWith recur o1 o2 -- unify common elements
      guard $ M.size match == M.size o1          -- intersection same size means ident keys
      return match

  (l@(C.T (TObject _)), r@(C.PartialObj _)) ->
    recur r l                                    -- flip args and recur
  (C.PartialObj p, SC.T (TObject o)) ->
    try $ SC.T <$> TObject <$> do
      guard $ M.isSubmapOfBy (\_ _ -> True) p o  -- is partial subset
      let update = M.intersectionWith recur o p  -- unify fields in common
      return $ M.union update o                  -- perform updates

  (C.PartialObj p1, SC.PartialObj p2) -> SC.PartialObj $ p1 `M.union` p2

  (C.TUnknown, other)      -> checkT env other
  (other,      SC.TUnknown) -> checkT env other

  (TError e1,  TError e2)  -> TError $ unifyError env substs e1 e2
  (a,          b)          -> TError $ TypeMismatch a b

  where recur = unifyWithSubsts env substs
        try :: Maybe SC.Type -> SC.Type
        try t = case t of
          Just t  -> t
          Nothing -> TError $ TypeMismatch _a _b

unifyError :: Env -> Map BindN BindN -> TypeError -> TypeError -> TypeError
unifyError env substs _a _b = case (_a, _b) of
  (TypeMismatch a1 a2, TypeMismatch b1 b2) -> TypeMismatch (mutRecur a1 b1) $ mutRecur a2 b2
  (CantCaseAnalyze a,  CantCaseAnalyze b)  -> CantCaseAnalyze $ mutRecur a b
  _                                        -> error "I DON'T EVEN"
  where mutRecur = unifyWithSubsts env substs

-}
