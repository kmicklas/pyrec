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
import qualified Pyrec.IR.ScopeCheck as SC
import           Pyrec.IR.ScopeCheck       hiding (E, T)
import qualified Pyrec.IR.TypeCheck  as TC
import           Pyrec.IR.TypeCheck               (Expr(..), Type(..), CType, RType)

deriving instance Ord BindN

type Entry = Decl TC.BindT BindN ()
type Env   = Map BindN Entry

type TC    = Maybe

type Pattern t = IR.Pattern t BindN

extendMap :: Map BindN a -> Map BindN a -> Map BindN a
extendMap = flip M.union

bindNParam :: BindN -> (BindN, Entry)
bindNParam k@(BN l i) = (k, Def Val (TC.BT l i $ TC.T TType) ())

bindTParam :: TC.BindT -> (BindN, Entry)
bindTParam b@(TC.BT l i _) = (BN l i , Def Val b ())

extendT :: TC.BindT -> Env -> Env
extendT = uncurry M.insert . bindTParam

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
    v'@(E _ vt' _) <- constrain env v vt
    let newDef x       = Def kind (TC.BT vl vi vt') x
        env'           = M.insert (BN vl vi) (newDef ()) env
    e'@(E _ t'  _) <- constrain env' e t
    return $ E l t' $ Let (newDef v') e'

  Let (Data i@(BN _ di) variants) e -> do
    -- | env + dummy type with same name. Just for typechecking variants.
    let envDummy = M.insert i (Data i []) env

    let fixVariant :: Variant BindT BindN -> TC (Variant TC.BindT BindN)
        fixVariant (Variant vi args) = Variant vi <$> (mapM . mapM) fixField args
          where fixField :: BindT -> TC TC.BindT
                fixField (BT bl bi t) = TC.BT bl bi <$> checkT envDummy t

    variants' <- mapM fixVariant variants

    let bindConstrs :: Variant TC.BindT BindN -> (BindN , Entry)
        bindConstrs (Variant b@(BN vl vi) ps) =
          (b, Def Val (TC.BT vl vi $ mkRT $ k $ TIdent $ Bound l di) ())
          where k = case ps of
                  Nothing     -> id
                  Just params -> TFun (for params $ \(TC.BT _ _ t) -> t) . mkRT


    let data' = Data i variants'
        env'  = extendMap envDummy $ M.fromList
                $ (i, data') : fmap bindConstrs variants'

    e'@(E _ t'  _) <- constrain env' e t
    return $ se t' $ Let data' e'

  Assign i@(Bound il ii) v -> do
    let t' = case M.lookup (BN il ii) env of
          Nothing -> error "internal: identifier was reported bound"
          Just (Def _ (TC.BT _ _ t') _) -> t'
          Just (Data  _              _) -> mkRT TType
    v'@(E _ t'' _) <- constrain env v $ rToC t'
    return $ se t'' $ Assign i v'

  Fun params body -> do
    -- returns mzero if no match, odd but convenient
    SC.T (TFun pts retT) <- return t

    let checkArg t1 (BT l i t2) = TC.BT l i <$> (checkT env =<< refineC env t1 t2)

    params'             <- join $ sequence <$> map2S checkArg pts params
    let env'             = extendMap env $ M.fromList $ bindTParam <$> params'
    body'@(E _ retT' _) <- constrain env' body $ retT
    let t'               = TC.T $ TFun ((\(TC.BT _ _ t) -> t) <$> params') retT'
    return $ se t' $ Fun params' body'

  FunT params body -> do
    SC.T (TForall inps retT) <- Just t
    guard $ length inps == length params
    let env'                  = extendMap env $ M.fromList $ bindNParam <$> params
    body'@(E _ retT' _)      <- constrain env' body $ retT
    let t'                    = TC.T $ TForall inps retT'
    return $ se t' $ FunT params body'

  App f args -> do
    let ft           = SC.T $ TFun (replicate (length args) TUnknown) t
    f'@(E _ ft' _)  <- constrain env f ft
    T (TFun pts t') <- Just $ ft'
    args'           <- join $ sequence <$> map2S (constrain env) args (rToC <$> pts)
    return $ se t' $ App f' args'

  AppT f args -> do
    let params                = for [1..length args] $ \n -> BN l $ 'T' : show n
        ft                    = SC.T $ TForall params TUnknown
    f'@(E _ ft' _)           <- constrain env f ft
    T (TForall params' retT) <- Just $ ft'
    args'                    <- mapM (checkT env) args
    substs                   <- M.fromList <$> map2S (,) params' args'
    t'                       <- cToR =<< unifyWithSubsts env substs t (rToC retT)
    return $ se t' $ AppT f' args'

  Cases vt v cases -> do
    v'@(E _ vt' _) <- constrain env v vt

    let bind :: RType -> Pattern BindT -> TC (Pattern TC.BindT , Env)
        bind t = \case

          (Binding b@(BT l i t')) -> do
            b' <- TC.BT l i <$> refineR env t' t
            return (Binding $ b', extendT b' M.empty )

          (Constr ci pats) -> do
            (Variant _ (params :: Maybe [TC.BindT])) <- do
              T (TIdent (Bound il ii)) <- return t
              let b                     = (BN il ii)
              Data b' vs               <- M.lookup b env
              guard $ b == b' -- don't want another type's constructor
              find (\(Variant vi _) -> vi == ci) vs

            -- outer TC signifies error, inner maybe is use to distinguish
            -- pyret's "| Constructor()" vs "| Constructor"
            let paramTs :: Maybe [RType] = (\(TC.BT _ _ t) -> t) <$$> params

            pats' :: Maybe [(Pattern TC.BindT , Env)] <- case (paramTs, pats) of
              (Nothing, Nothing) -> return Nothing
              (Just ts, Just ps) -> Just <$> join $ sequence
                                    <$> map2S bind ts ps
              _                  -> mzero

            let env' = extendMap env $ M.unions $ fromMaybe [] $ snd <$$> pats'
            return (Constr ci $ fst <$$> pats' , env')

    let checkCase :: CType -> Case BindT BindN SC.Expr
                  -> TC (Case TC.BindT BindN TC.Expr, CType)
        checkCase t (Case pat body) = do
          (pat' , env')     <- bind vt' pat
          body'@(E _ t'  _) <- constrain env' body t
          return (Case pat' body', rToC t')

    (cases', t') <- mapAccumM checkCase t cases
    se <$> cToR t' <*> return (Cases vt' v' cases')

  EmptyObject -> se <$> (refineR env t $ T $ TObject M.empty)
                    <*> return EmptyObject

  Extend obj fi fv -> do
    let f m          = M.lookup fi m
    ft               <- case t of
      SC.T (TObject o) -> f o
      SC.PartialObj p  -> f p
      _                -> mzero
    fv' @(E _ ft' _) <- constrain env fv ft

    let f' m          = M.delete fi m
    ot               <- case t of
      SC.T (TObject o) -> return $ SC.T $ TObject $ f' o
      SC.PartialObj p  -> return $ SC.PartialObj  $ f' p
      _                -> mzero
    obj'@(E _ ot' _) <- constrain env obj ot

    t'               <- case ot' of
      T (TObject o)    -> return $ T $ TObject $ M.insert fi ft' o
      _                -> mzero
    return $ se t' $ Extend obj' fi fv'

  Access obj fi -> do
    let ot             = SC.PartialObj $ M.singleton fi t
    obj'@(E _ ot'  _) <- constrain env obj ot
    T (TObject fs)    <- return ot'
    t'                <- M.lookup fi fs
    return $ se t' $ Access obj' fi

  where se = E l



rToC :: RType -> CType
rToC (T inner) = SC.T $ rToC <$> inner

cToR :: CType -> TC RType
cToR = \case
  SC.T inner   -> TC.T <$> mapM cToR inner
  PartialObj _ -> mzero
  TUnknown     -> mzero

checkT :: Env -> CType -> TC RType
checkT env t = cToR =<< checkCT env t

checkCT :: Env -> CType -> TC CType
checkCT env = \case
  t@(SC.T (TIdent (Bound l i))) -> case M.lookup (BN l i) env of
    Just (Def Val (TC.BT _ _ (TC.T TType)) _) -> return t
    Just (Data    _                        _) -> return t
    _                                         -> mzero

  SC.T (TForall params retT)  -> checkCT env' retT
    where env' = extendMap env $ M.fromList $ bindNParam <$> params

  SC.T inner                  -> SC.T       <$> mapM (checkCT env) inner
  PartialObj inner            -> PartialObj <$> mapM (checkCT env) inner
  t@(TUnknown)                -> return t

-- refines a result type
refineR :: Env -> CType -> RType -> TC RType
refineR env l r = checkT env =<< (refineC env l $ rToC r)

-- refines a constraint type
refineC :: Env -> CType -> CType -> TC CType
refineC env l r = unifyWithSubsts env M.empty l r

-- expected then got
unifyWithSubsts :: Env -> Map BindN RType -> CType -> CType -> TC CType
unifyWithSubsts env substs l r = case (l, r) of
  (SC.T TType, SC.T TType) -> return $ SC.T TType

  (SC.T (TIdent (Bound bl bi)), _) -> case M.lookup key substs of
    -- either a substitutes for b, and we recur with that
    Just l'  -> unifyWithSubsts env M.empty (rToC l') r
    -- or a has no substitution and a must equal b
    Nothing  -> do guard $ l == r
                   return l
    where key = BN bl bi

  (SC.T (TFun aParams aRes), SC.T (TFun bParams bRes)) -> SC.T <$> do
    params <- join $ sequence <$> map2S recur aParams bParams
    TFun params <$> recur aRes bRes

  (SC.T (TForall aParams aRes), SC.T (TForall bParams bRes)) -> SC.T <$> do
    let f a (BN l i) = (a , TC.T $ TIdent $ Bound l i)
    news            <- map2S f aParams bParams
    let substs'      = extendMap substs $ M.fromList news
    TForall aParams <$> unifyWithSubsts env substs' aRes bRes

  ((SC.T (TObject o1)), (SC.T (TObject o2))) -> SC.T <$> TObject <$> do
    guard $ M.size o1 == M.size o2             -- same size
    let match = M.intersectionWith recur o1 o2 -- unify common elements
    guard $ M.size match == M.size o1          -- intersection same size means ident keys
    sequence match

  ((SC.T (TObject _)), (PartialObj _)) ->
    recur r l                                  -- flip args and recur

  (PartialObj p, SC.T (TObject o)) -> SC.T <$> TObject <$> do
    guard $ M.isSubmapOfBy (\_ _ -> True) p o  -- is partial subset
    let update = M.intersectionWith recur o p  -- unify fields in common
    M.union <$> sequence update <*> return o   -- perform updates

  (PartialObj p1, PartialObj p2) -> return $ PartialObj $ p1 `M.union` p2

  (TUnknown,   other)     -> checkCT env other
  (other,      TUnknown)  -> checkCT env other

  (a,          b)         -> mzero

  where recur = unifyWithSubsts env substs
