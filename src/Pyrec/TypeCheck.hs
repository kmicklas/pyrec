{-# LANGUAGE StandaloneDeriving #-}

module Pyrec.TypeCheck where

import           Prelude                   hiding (mapM,       sequence)

import           Control.Applicative
import           Control.Monad             hiding (mapM, forM, sequence)
import           Control.Monad.Reader      hiding (mapM, forM, sequence)

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

type Entry   = Decl TC.BindT BindN ()
type Env     = Map BindN Entry

type TC      = Maybe

type Pattern = IR.Pattern RType BindN

extendMap :: Map BindN a -> Map BindN a -> Map BindN a
extendMap = flip M.union

bindNParam :: BindN -> (BindN, Entry)
bindNParam k@(BN l i) = (k, Def Val (TC.BT l i $ TC.T TType) ())

bindTParam :: TC.BindT -> (BindN, Entry)
bindTParam b@(TC.BT l i _) = (BN l i , Def Val b ())

--extendT :: BindT -> Map BindN Entry -> Map BindN Entry
--extendT = uncurry M.insert . bindTParam

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
typeCheckH env (SC.E          l _ e) t = case e of

  -- Need too look up to get proper loc
  Num n -> se <$> (refineR env t $ TC.T $ TIdent $ Bound Intrinsic "Number")
              <*> (return $ Num n)
  Str s -> se <$> (refineR env t $ TC.T $ TIdent $ Bound Intrinsic "String")
              <*> (return $ Str s)

  Ident i@(Bound il ii) -> se <$> (refineR env t it') <*> (return $ Ident i)
    where it' :: RType = case M.lookup (BN il ii) env of
            Nothing -> error "internal: identifier was reported bound"
            Just (Def _ (TC.BT _ _ t') _) -> t'
            Just (Data  _              _) -> mkRT TType

  Let (Def kind b@(BT vl vi vt) v) e -> do
    v'@(TC.E _ vt' _) <- constrain env v vt
    let newDef x       = Def kind (TC.BT vl vi vt') x
        env'           = M.insert (BN vl vi) (newDef ()) env
    e'@(TC.E _ t'  _) <- constrain env' e t
    return $ TC.E l t' $ Let (newDef v') e'

{-
  Let (Data i@(BN _ di) variants) e -> do
    let fixVariant :: Variant BindT BindN -> Variant BindT BindN
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

    se t' $ Let data' e'
-}
  Assign i@(Bound il ii) v -> do
    let t' = case M.lookup (BN il ii) env of
          Nothing -> error "internal: identifier was reported bound"
          Just (Def _ (TC.BT _ _ t') _) -> t'
          Just (Data  _              _) -> mkRT TType
    v'@(TC.E _ t'' _) <- constrain env v $ rToC t'
    return $ se t'' $ Assign i v'

  Fun params body -> do
    T (TFun pts retT)      <- Just t -- returns Nothing if no match, odd but convenient

    let checkArg t1 (BT l i t2) = TC.BT l i <$> (checkT env =<< refineC env t1 t2)

    params'                <- join $ sequence <$> map2S checkArg pts params
    let env'                = extendMap env $ M.fromList $ bindTParam <$> params'
    body'@(TC.E _ retT' _) <- constrain env' body $ retT
    let t'                  = TC.T $ TFun ((\(TC.BT _ _ t) -> t) <$> params') retT'
    return $ se t' $ Fun params' body'

  FunT params body -> do
    T (TParam inps retT)   <- Just t
    guard $ length inps == length params
    let env'                = extendMap env $ M.fromList $ bindNParam <$> params
    body'@(TC.E _ retT' _) <- constrain env' body $ retT
    let t'                  = TC.T $ TParam inps retT'
    return $ se t' $ FunT params body'

  App f args -> do
    let ft              = T $ TFun (replicate (length args) TUnknown) t
    f'@(TC.E _ ft' _)  <- constrain env f ft
    TC.T (TFun pts t') <- Just $ ft'
    args'              <- join $ sequence <$> map2S (constrain env) args (rToC <$> pts)
    return $ se t' $ App f' args'
{-
  AppT f args -> do
    let params = for [1..length args] $ \n -> BN l $ 'T' : show n
        ft                     = T $ TParam params TUnknown
    f'@(TC.E _ ft' _)          <- constrain env f ft
    TC.T (TParam params' retT) <- Just $ ft'
    args'                      <- mapM (checkT env) args
    substs                     <- join $ sequence <$> map2S (,) params' args'
    t'                         <- _ env t $ _ (M.fromList substs) retT
    return $ se t' $ AppT f' args'

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
-}
  where se = TC.E l

{-
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
-}

rToC :: RType -> CType
rToC (TC.T inner) = T $ rToC <$> inner

cToR :: CType -> TC RType
cToR = \case
  T inner      -> TC.T <$> mapM cToR inner
  PartialObj _ -> mzero
  TUnknown     -> mzero

checkT :: Env -> CType -> TC RType
checkT env t = cToR =<< checkCT env t

checkCT :: Env -> CType -> TC CType
checkCT env = \case
  t@(T (TIdent (Bound l i))) -> case M.lookup (BN l i) env of
    Just (Def Val (TC.BT _ _ (TC.T TType)) _) -> return t
    Just (Data    _                        _) -> return t
    _                                         -> mzero

  T (TParam params retT)     -> checkCT env' retT
    where env' = extendMap env $ M.fromList $ bindNParam <$> params

  T inner                    -> T          <$> mapM (checkCT env) inner
  PartialObj inner           -> PartialObj <$> mapM (checkCT env) inner
  t@(TUnknown)               -> return t

-- refines a result type
refineR :: Env -> CType -> RType -> TC RType
refineR env l r = checkT env =<< (refineC env l $ rToC r)

-- refines a constraint type
refineC :: Env -> CType -> CType -> TC CType
refineC env l r = unifyWithSubsts env M.empty l r

-- expected then got
unifyWithSubsts :: Env -> Map BindN RType -> CType -> CType -> TC CType
unifyWithSubsts env substs l r = case (l, r) of
  (T TType, T TType) -> return $ T TType

  (T (TIdent (Bound bl bi)), _) -> case M.lookup key substs of
    -- either a substitutes for b, and we recur with that
    Just l'  -> unifyWithSubsts env M.empty (rToC l') r
    -- or a has no substitution and a must equal b
    Nothing  -> do guard $ l == r
                   return l
    where key = BN bl bi

  (T (TFun aParams aRes), T (TFun bParams bRes)) -> T <$> do
    params <- join $ sequence <$> map2S recur aParams bParams
    TFun params <$> recur aRes bRes

--  (T (TParam aParams aRes), T (TParam bParams bRes)) -> T <$> do
--    news <- map2S (,) aParams bParams
--    let substs' = extendMap substs $ M.fromList news
--    return $ TParam aParams $ unifyWithSubsts env substs' aRes bRes

  ((T (TObject o1)), (T (TObject o2))) -> T <$> TObject <$> do
    guard $ M.size o1 == M.size o2             -- same size
    let match = M.intersectionWith recur o1 o2 -- unify common elements
    guard $ M.size match == M.size o1          -- intersection same size means ident keys
    sequence match

  ((T (TObject _)), (PartialObj _)) ->
    recur r l                                  -- flip args and recur
  (PartialObj p, T (TObject o)) -> T <$> TObject <$> do
    guard $ M.isSubmapOfBy (\_ _ -> True) p o  -- is partial subset
    let update = M.intersectionWith recur o p  -- unify fields in common
    M.union <$> sequence update <*> return o   -- perform updates

  (PartialObj p1, PartialObj p2) -> return $ PartialObj $ p1 `M.union` p2

  (TUnknown,   other)     -> checkCT env other
  (other,      TUnknown)  -> checkCT env other

  (a,          b)         -> mzero

  where recur = unifyWithSubsts env substs
