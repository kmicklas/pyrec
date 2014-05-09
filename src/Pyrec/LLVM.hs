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
import LLVM.General.AST.CallingConvention

import qualified LLVM.General.AST as AST (Name(..))

type LName = AST.Name

type LLVM = RWS () ([Definition], [Named Instruction]) Word

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

lname :: Name -> LName
lname = \case
  Name n p -> AST.Name $ n ++ "$" ++ showLoc p
  Gen n    -> AST.UnName n

llvm :: Expr -> LLVM LName
llvm = \case
  App f args (rc, ec) ->
    do res <- gen
       instr $ res := Call True Fast [] (Right $ op f)
                           (((,[]) . op) <$> args) [] []
       return res
    where op (Var n) = LocalReference $ lname n
  Fix fns e ->
    do let closureVars = toList $ (fixFrees mempty) fns
       fids <- sequence $ llvmFun closureVars <$> fns
       env <- llvmEnv closureVars
       sequence $ llvmClosure env <$>
         zip (lname <$> fids) (funName <$> fns)
       llvm e

llvmFun :: [Name] -> Fun -> LLVM Name
llvmFun cvs (Fun n args (rc, ec) e) = undefined

llvmEnv :: [Name] -> LLVM Name
llvmEnv ns = undefined

llvmClosure :: Name -> (LName, Name) -> LLVM ()
llvmClosure env (ln, n) = undefined
