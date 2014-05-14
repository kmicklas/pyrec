module Pyrec.LLVM where

import           Prelude                  hiding (map, mapM, mapM_)

import           Control.Applicative
import           Control.Monad.RWS        hiding (mapM, mapM_, sequence)

import           Data.Set
import           Data.Monoid
import           Data.Word
import           Data.Foldable            hiding (toList)
import           Data.Traversable         hiding (sequence)

import qualified LLVM.General.AST         as AST
import           LLVM.General.AST         hiding (Name)
import           LLVM.General.AST.AddrSpace
import           LLVM.General.AST.CallingConvention
import           LLVM.General.AST.Constant
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

llvmModule :: String -> String -> [Definition] -> Expr -> Module
llvmModule modName entryName decls e =
  Module modName Nothing Nothing (decls ++ (main : defs))
  where (_, _, (defs, block)) = runRWS (llvmExpr e) () 0
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
  Var n    -> singleton n
  Cont a e -> exprFrees e \\ singleton a
  _        -> mempty

funsFrees :: [Fun] -> Set Name
funsFrees fns = foldMap funFrees fns \\ foldMap (singleton . funName) fns
  where funFrees :: Fun -> Set Name
        funFrees (Fun _ as (rc, ec) e) = exprFrees e \\ fromList (rc : ec : as)

instr :: Named Instruction -> LLVM ()
instr i = tell $ ([],) $ return $ i

call f args = Call True pyrecConvention [] (Right f)
                        ((,[]) <$> args) [] []

localName :: String -> Word -> LName
localName n w = lname $ n ++ "$" ++ show w

operand :: Val -> LLVM Operand
operand = \case
  Var n -> return $ case n of
    (Name n Intrinsic)  -> ConstantOperand $ GlobalReference
                           $ lname $ "pyrec" ++ n
    (Name n (User _ w)) -> LocalReference
                           $ lname $ n ++ "$" ++ show w
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
    let closureVars = toList $ valFrees c
    kid <- llvmFun closureVars [arg] body
    env <- llvmEnv closureVars
    llvmClosure env kid
    return $ LocalReference kid

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
    let closureVars = toList $ funsFrees fns
    let lf :: Fun -> LLVM LName
        lf (Fun _ args (rk, ek) body) =
          llvmFun closureVars (rk : ek : args) body
    fids <- mapM lf fns
    env  <- llvmEnv closureVars
    mapM_ (llvmClosure env) fids
    llvmExpr e

llvmFun :: [Name] -> [Name] -> Expr -> LLVM LName
llvmFun cvs args e =
  do (_, (ds, is)) <- censor (const ([], [])) $ listens id $ llvmExpr e
     tell (ds, [])
     res <- gen
     tell $ (,[]) $ return $ GlobalDefinition $
       Function Private
                Hidden
                pyrecConvention
                []
                VoidType
                res
                (llvmParam <$> args, False)
                []
                Nothing
                0
                Nothing
                [BasicBlock (lname "entry") is $ Do $ Unreachable  []]
     return res
  where llvmParam :: Name -> Parameter
        llvmParam n = (flip $ Parameter pVal) [] $ lname $ case n of
          (Name n Intrinsic)  -> "pyrec" ++ n
          (Name n (User _ w)) -> n ++ "$" ++ show w

llvmEnv :: [Name] -> LLVM LName
llvmEnv ns = undefined

llvmClosure :: LName -> LName -> LLVM ()
llvmClosure env fid = undefined

pVal :: Type
pVal = PointerType (IntegerType 8) $ AddrSpace 0
