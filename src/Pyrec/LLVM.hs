module Pyrec.LLVM where

import           Prelude                  hiding (map, mapM, mapM_, forM_, forM, sequence)

import           Control.Applicative
import           Control.Monad.RWS        hiding (mapM, mapM_, forM_, forM, sequence)

import           Data.Set                 as S
import           Data.Monoid
import           Data.Word
import           Data.Foldable            hiding (toList)
import           Data.Traversable         hiding (for)

import qualified LLVM.General.AST         as AST
import           LLVM.General.AST         hiding (Name)
import           LLVM.General.AST.AddrSpace
import           LLVM.General.AST.Attribute
import           LLVM.General.AST.CallingConvention
import           LLVM.General.AST.Constant hiding (BitCast, GetElementPtr)
import           LLVM.General.AST.Float
import           LLVM.General.AST.Linkage
import           LLVM.General.AST.Visibility

import           Pyrec.Misc
import           Pyrec.CPS

type LName = AST.Name
lname      = AST.Name

type LLVM = RWS () ([Definition], [Named Instruction]) Word


pyrecConvention :: CallingConvention
pyrecConvention = C

llvmModule :: Word -> String -> String -> [Definition] -> Expr -> Module
llvmModule count modName entryName decls e =
  Module modName Nothing Nothing (decls ++ (main : defs))
  where (_, _, (defs, block)) = runRWS (llvmExpr e) () count
        main = GlobalDefinition
               $ Function External
                          Default
                          pyrecConvention
                          []
                          VoidType
                          (lname entryName)
                          ([], False)
                          []
                          Nothing
                          0
                          Nothing
                          [BasicBlock (lname "moduleEntry") block
                           $ Do $ Unreachable []]

gen :: LLVM LName
gen = UnName <$> get <* modify (+ 1)

funName :: Fun -> Name
funName (Fun n _ _ _) = n

exprFrees :: Expr -> Set Name
exprFrees = \case
  App f as (rc, ec) -> foldMap valFrees $ f : rc : ec : as
  Continue k e      -> valFrees k `mappend` valFrees e
  Fix fns e         -> funsFrees fns `mappend` exprFrees e

valFrees :: Val -> Set Name
valFrees = \case
  Var (Name _ Intrinsic) -> mempty -- we don't want to close over globals
  Var n                  -> singleton n
  Cont a e               -> exprFrees e \\ singleton a
  _                      -> mempty

funsFrees :: [Fun] -> Set Name
funsFrees fns = foldMap funFrees fns \\ foldMap (singleton . funName) fns
  where funFrees :: Fun -> Set Name
        funFrees (Fun _ as (rc, ec) e) = exprFrees e \\ fromList (rc : ec : as)

instr :: Named Instruction -> LLVM ()
instr i = tell $ ([],) $ return $ i

call f args = Call True pyrecConvention [] (Right f)
                   ((,[]) <$> args) [] []

llvmName :: Name -> LName
llvmName = \case
  Name n Intrinsic  -> lname $ "pyrec" ++ n
  Name n (User _ w) -> lname $ n ++ "$" ++ show w
  Gen w             -> UnName w

operand :: Val -> LLVM Operand
operand = \case
  Var n -> return $ case n of
    Name _ Intrinsic  -> ConstantOperand $ GlobalReference $ llvmName n
    _                 -> LocalReference                    $ llvmName n
  Num n -> do
    res <- gen
    let fop = ConstantOperand $ GlobalReference
              $ lname "pyrecLoadNumber"
    instr $ res := call fop [ConstantOperand $ Float $ Double n]
    return $ LocalReference res
  Str r -> do
    res <- gen
    let fop = ConstantOperand $ GlobalReference
              $ lname "pyrecLoadString"
    instr $ res := call fop [_]
    return $ LocalReference res
  c@(Cont arg body) -> do
    let closedVars = toList $ valFrees c
    env <- llvmClosure closedVars
    LocalReference <$> llvmFun env closedVars [arg] body


llvmExpr :: Expr -> LLVM LName
llvmExpr = \case
  App f args (rc, ec) -> do
    res  <- gen
    fop  <- operand f
    aops <- mapM operand (rc : ec : args)
    instr $ res := call fop aops
    return res

  Continue k a -> do
    res <- gen
    kop <- operand k
    aop <- operand a
    instr $ res := call kop [aop]
    return res

  Fix fns e -> do
    let closedVars = toList $ funsFrees fns
    env <- llvmClosure closedVars
    let lf :: Fun -> LLVM LName
        lf (Fun _ args (rk, ek) body) =
          llvmFun env closedVars (rk : ek : args) body
    mapM_ lf fns
    llvmExpr e

llvmFun :: LName -> [Name] -> [Name] -> Expr -> LLVM LName
llvmFun env cvs args e = do
  s <- get
  let (_, s', (defs, block)) = runRWS (llvmExpr e) () s
  put s'

  tell (defs, [])

  let envParam = Parameter (ptr pVal) (lname "env") [Nest]

  closureHeader <- fmap join $ forM (zip [0..] closedLNames) $ \(idx, name) -> do
        loc <- gen
        return $ [ loc  :=
                   GetElementPtr True (LocalReference $ lname "env")
                                 [mkConstant idx] []
                 , name :=
                   Load False (LocalReference loc) Nothing 8 []
                 ]

  funName <- gen
  tell $ (,[]) $ return $ GlobalDefinition $
    Function Private
             Hidden
             pyrecConvention
             []
             VoidType
             funName
             (envParam : ((flip $ Parameter pVal) [] <$> llvmName' <$> args), False)
             []
             Nothing
             0
             Nothing
             [BasicBlock (lname "entry") (closureHeader ++ block)
              $ Do $ Unreachable  []]

  tramp <- gen
  instr $ tramp := Alloca (IntegerType 8) (Just $ mkConstant 72) 16 []
  instr $ Do $ Call True pyrecConvention []
    (Right $ ConstantOperand $ GlobalReference $ lname "llvm.init.trampoline")
    ((,[]) <$> [ LocalReference tramp
               , ConstantOperand $ GlobalReference $ funName
               , LocalReference env ]) [] []

  tramp' <- gen
  instr $ tramp' := Call True pyrecConvention []
    (Right $ ConstantOperand $ GlobalReference $ lname "llvm.adjust.trampoline")
    ((,[]) <$> [ LocalReference tramp ]) [] []
  instr $ funName := BitCast (LocalReference tramp')
    (ptr $ FunctionType VoidType (for args $ const pVal) False) []
  return funName

  where closedLNames = llvmName' <$> cvs
        llvmName' = \case
          Name _ Intrinsic ->
            error "how could function params or closed-over vars be globals?"
          n                -> llvmName n

llvmClosure :: [Name] -> LLVM LName
llvmClosure names = do
  let len = length names
  res <- gen
  instr $ res := Alloca pVal (Just $ mkConstant $ fromIntegral len)  0 []
  forM_ (zip [0..] names) $ \(idx, name) -> do
    loc <- gen
    instr $ loc := GetElementPtr True (LocalReference res) [mkConstant idx] []
    op <- operand $ Var name
    instr $ Do $ Store False (LocalReference loc) op Nothing 0 []
  return res

mkConstant = ConstantOperand . Int 32

ptr t = PointerType t $ AddrSpace 0

pVal :: Type
pVal = ptr $ IntegerType 8
