module Pyrec.LLVM where

import Data.Set
import Data.Word
import Data.Monoid
import Data.Foldable hiding (toList)

import Control.Monad.RWS
import Control.Applicative

import Pyrec.Misc
import Pyrec.CPS

import LLVM.General.AST hiding (Name)
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant
import LLVM.General.AST.Float
import LLVM.General.AST.Linkage
import LLVM.General.AST.Visibility

import qualified LLVM.General.AST as AST (Name(..))

type LName = AST.Name
lname      = AST.Name

type LLVM = RWS () ([Definition], [Named Instruction]) Word

pyrecConvention :: CallingConvention
pyrecConvention = C

llvmModule :: String -> String -> [Definition] -> Expr -> Module
llvmModule modName entryName decls e =
  Module modName Nothing Nothing (decls ++ (main : defs))
  where (r, _, (defs, block)) = runRWS (llvmExpr e) () 0
        main = GlobalDefinition
               $ Function External
                          Default
                          pyrecConvention
                          []
                          (IntegerType 32)
                          (lname entryName)
                          ([], False)
                          []
                          Nothing
                          0
                          Nothing
                          [BasicBlock (lname "moduleEntry") block
                           $ Do $ Ret (Just $ LocalReference r) []]

gen :: LLVM LName
gen = UnName <$> get <* modify (+ 1)

funName :: Fun -> Name
funName (Fun n _ _ _) = n

frees :: Set Name -> Expr -> Set Name
frees env = \case
  App f as (rc, ec) -> foldMap (valFrees env) $ f : rc : ec : as
  Fix fns e         -> fixFrees env fns `mappend` frees env' e
    where env' = mappend env $ foldMap (singleton . funName) fns

valFrees :: Set Name -> Val -> Set Name
valFrees env = \case
  Var n -> if member n env then mempty else singleton n
  _     -> mempty

fixFrees :: Set Name -> [Fun] -> Set Name
fixFrees env fns = foldMap (funFrees env') fns
  where env' = mappend env $ foldMap (singleton . funName) fns

funFrees :: Set Name -> Fun -> Set Name
funFrees env (Fun _ as (rc, ec) e) = frees env' e
  where env' = mappend env $ fromList $ rc : ec : as

instr :: Named Instruction -> LLVM ()
instr i = tell $ ([],) $ return $ i

call f args = Call True pyrecConvention [] (Right f)
                        ((,[]) <$> args) [] []

localName :: String -> Word -> LName
localName n w = lname $ n ++ "$" ++ show w

operand :: Val -> LLVM Operand
operand = \case
  Var n -> return $ case n of
    (Name n Intrinsic)  -> ConstantOperand $ GlobalReference $ lname n
    (Name n (User _ w)) -> LocalReference  $ localName n w
  Num n -> do
    res <- gen
    let fop = ConstantOperand $ GlobalReference
              $ lname "@pyrecLoadNumber"
    instr $ res := call fop [ConstantOperand $ Float $ Double n]
    return $ LocalReference res
  Str r -> do
    res <- gen
    let fop = ConstantOperand $ GlobalReference
              $ lname "@pyrecLoadString"
    instr $ res := call fop [_]
    return $ LocalReference res

llvmExpr :: Expr -> LLVM LName
llvmExpr = \case
  App f args (rc, ec) -> do
    res  <- gen
    fop  <- operand f
    aops <- mapM operand (rc : ec : args)
    instr $ res := call fop aops
    return res

  Cont k a -> do
    res <- gen
    kop <- operand k
    aop <- operand a
    instr $ res := call kop [aop]
    return res

  Fix fns e -> do
    let closureVars = toList $ (fixFrees mempty) fns
    fids <- sequence $ llvmFun closureVars <$> fns
    env <- llvmEnv closureVars
    sequence $ llvmClosure env <$>
      zip fids (funName <$> fns)
    llvmExpr e

llvmFun :: [Name] -> Fun -> LLVM LName
llvmFun cvs (Fun n args (rc, ec) e) =
  do (r, (ds, is)) <- censor (const ([], [])) $ listens id $ llvmExpr e
     tell (ds, [])
     tell $ (,[]) $ return $ GlobalDefinition $
       Function Private
                Hidden
                pyrecConvention
                []
                pVal
                n''
                (params, False)
                []
                Nothing
                0
                Nothing
                [BasicBlock (lname "entry") is $ Do $ Ret (Just $ LocalReference r) []]
     return n''
  where (Name n' (User _ w)) = n
        n''                  = localName n' w
        params               = _

llvmEnv :: [Name] -> LLVM Name
llvmEnv ns = undefined

llvmClosure :: Name -> (LName, Name) -> LLVM ()
llvmClosure env (ln, n) = undefined

pVal :: Type
pVal = PointerType (IntegerType 8) $ AddrSpace 0
