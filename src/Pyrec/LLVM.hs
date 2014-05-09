module Pyrec.LLVM where

import Data.Set
import Data.Word

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
gen = UnName <$> get <* modify (\ n -> n + 1)

frees :: Set Name -> Expr -> Set Name
frees = undefined

fixFrees :: [Fun] -> Set Name
fixFrees = undefined

instr :: Named Instruction -> LLVM ()
instr i = undefined

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
    do let closureVars = toList $ fixFrees fns
       fids <- sequence $ llvmFun closureVars <$> fns
       env <- llvmEnv closureVars
       sequence $ llvmClosure env <$>
         zip (lname <$> fids) (for fns $ \ (Fun n _ _ _) -> n)
       llvm e

llvmFun :: [Name] -> Fun -> LLVM Name
llvmFun cvs (Fun n args (rc, ec) e) = undefined

llvmEnv :: [Name] -> LLVM Name
llvmEnv ns = undefined

llvmClosure :: Name -> (LName, Name) -> LLVM ()
llvmClosure env (ln, n) = undefined
