module Pyrec.Emit where

import qualified Data.Map as M
import           Data.Map (Map)

import Pyrec.SSA

header =
  "%pyretVal = type i8*\n\n" ++
  "declare %pyretVal @loadPyretNumber(double *)\n\n"

emit :: Module -> String
emit m = header ++ (M.assocs m >>= ((++ "\n\n") . emitConstant m))

emitConstant :: Module -> (Id, Constant) -> String

emitConstant m (id, Num n) = id ++ " = constant double " ++ show n

emitConstant m (id, Fun params blocks) =
  "define %pyretVal " ++ id ++ "(" ++ emitParams params ++ ") {\n" ++
  emitBlocks m 0 blocks ++ "}"

emitParams :: [Id] -> String
emitParams [] = ""
emitParams [p] = "%pyretVal " ++ p
emitParams (p : ps) = "%pyretVal " ++ p ++ ", " ++ emitParams ps

emitBlocks :: Module -> Int -> [Block] -> String
emitBlocks _ _ [] = ""
emitBlocks m n ((Block binds j) : bs) =
  "block" ++ show n ++ ":\n" ++
  (binds >>= emitStmt m) ++ emitJump j ++ "\n" ++
  emitBlocks m (n + 1) bs 

emitStmt :: Module -> Statement -> String

emitStmt _ (Bind id (Atomic (Bound bid))) =
  "\t" ++ id ++ " = bitcast %pyretVal " ++ bid ++ " to %pyretVal\n"

emitStmt m (Bind id (Atomic (Const cid))) =
  "\t" ++ id ++ " = " ++ case M.lookup cid m of
    Just (Num n) -> "call %pyretVal @loadPyretNumber(double* " ++ cid ++ ")\n" 
    Nothing      -> error "Internal error: bad constant"

emitStmt m (Bind id Alloca) = "\t" ++ id ++ " = alloca %pyretVal*\n"

emitStmt m (Bind id (Load lid)) =
  "\t" ++ id ++ " = load %pyretVal* " ++ lid ++ "\n"

emitStmt m (Assign id (Bound bid)) =
  "\tstore %pyretVal " ++ bid ++ ", %pyretVal* " ++ id ++ "\n"

emitJump :: Jump -> String
emitJump (Return (Bound id)) = "\tret %pyretVal " ++ id
