{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module CodeGen where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.CallingConvention as CC

import LLVM.AST (Named((:=)))
import LLVM.AST.Instruction (FastMathFlags(..))

import qualified Data.ByteString.Char8 as BS
import Control.Monad.State
import Data.Word
import Data.List

import AbsBF

-- | LLVM code generation state
data CodegenState = CodegenState
  { currentBlock :: AST.Name
  , blocks       :: [BasicBlock]
  , blockCount   :: Int
  , count        :: Word
  , names        :: Names
  } deriving Show

type Names = [(String, Int)]

data BasicBlock = BasicBlock
  { blockName :: AST.Name
  , blockStack :: [Named AST.Instruction]
  , blockTerm  :: Maybe (Named AST.Terminator)
  } deriving Show

type Codegen = State CodegenState

-- | Generate LLVM module from Brainfuck AST
codegenModule :: BF -> AST.Module
codegenModule bf = AST.defaultModule
  { AST.moduleName = "brainfuck"
  , AST.moduleDefinitions = [mainDef, putcharDef, getcharDef]
  }
  where
    mainDef = AST.GlobalDefinition $ G.functionDefaults
      { G.name = AST.Name "main"
      , G.returnType = T.i32
      , G.basicBlocks = createBlocks $ execCodegen $ do
          entry <- addBlock "entry"
          setBlock entry
          
          -- Allocate memory tape (30000 cells)
          tape <- alloca (T.ArrayType 30000 T.i8)
          
          -- Initialize tape to zeros
          zero <- iconst 0
          tapePtr <- gep tape [zero, zero]
          call (externf "llvm.memset.p0i8.i32") 
               [tapePtr, zero, iconst 30000, iconst 0, iconst 0]
          
          -- Current pointer starts at beginning
          ptr <- alloca T.i32
          store ptr zero
          
          -- Generate code for BF program
          codegenBF bf tape ptr
          
          -- Return 0
          ret (iconst 0)
      }

    putcharDef = AST.GlobalDefinition $ G.functionDefaults
      { G.name = AST.Name "putchar"
      , G.returnType = T.i32
      , G.parameters = ([G.Parameter T.i32 (AST.Name "c") []], False)
      , G.basicBlocks = []
      }

    getcharDef = AST.GlobalDefinition $ G.functionDefaults
      { G.name = AST.Name "getchar"
      , G.returnType = T.i32
      , G.parameters = ([], False)
      , G.basicBlocks = []
      }

-- | Generate LLVM IR for Brainfuck statements
codegenBF :: BF -> AST.Operand -> AST.Operand -> Codegen ()
codegenBF (BFGrammar stms) tape ptr = mapM_ (codegenStm tape ptr) stms

codegenStm :: AST.Operand -> AST.Operand -> Stm -> Codegen ()
codegenStm tape ptr stm = case stm of
  SIncrement -> do
    -- ptr++
    idx <- load ptr
    one <- iconst 1
    idx' <- add idx one
    store ptr idx'

  SDecrement -> do
    -- ptr--
    idx <- load ptr
    one <- iconst 1
    idx' <- sub idx one
    store ptr idx'

  SIncBytAtP -> do
    -- tape[ptr]++
    idx <- load ptr
    zero <- iconst 0
    cellPtr <- gep tape [zero, idx]
    val <- load cellPtr
    one8 <- iconst8 1
    val' <- add val one8
    store cellPtr val'

  SDecBytAtP -> do
    -- tape[ptr]--
    idx <- load ptr
    zero <- iconst 0
    cellPtr <- gep tape [zero, idx]
    val <- load cellPtr
    one8 <- iconst8 1
    val' <- sub val one8
    store cellPtr val'

  SOutput -> do
    -- putchar(tape[ptr])
    idx <- load ptr
    zero <- iconst 0
    cellPtr <- gep tape [zero, idx]
    val <- load cellPtr
    val32 <- zext val T.i32
    call (externf "putchar") [val32]
    return ()

  SInput -> do
    -- tape[ptr] = getchar()
    idx <- load ptr
    zero <- iconst 0
    cellPtr <- gep tape [zero, idx]
    val <- call (externf "getchar") []
    val8 <- trunc val T.i8
    store cellPtr val8

  SWhile stms -> mdo
    -- while (tape[ptr] != 0) { ... }
    idx <- load ptr
    zero <- iconst 0
    cellPtr <- gep tape [zero, idx]
    val <- load cellPtr
    zero8 <- iconst8 0
    cond <- icmp IP.NE val zero8
    
    condBr cond loopBody loopExit
    
    loopBody <- addBlock "loop.body"
    setBlock loopBody
    mapM_ (codegenStm tape ptr) stms
    
    -- Check condition again
    idx' <- load ptr
    cellPtr' <- gep tape [zero, idx']
    val' <- load cellPtr'
    cond' <- icmp IP.NE val' zero8
    condBr cond' loopBody loopExit
    
    loopExit <- addBlock "loop.exit"
    setBlock loopExit

-- | Codegen monad helpers
emptyCodegen :: CodegenState
emptyCodegen = CodegenState (AST.Name "entry") [] 1 0 []

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState m emptyCodegen

freshName :: Codegen Word
freshName = do
  i <- gets count
  modify $ \s -> s { count = i + 1 }
  return i

fresh :: Codegen AST.Name
fresh = AST.UnName <$> freshName

addBlock :: String -> Codegen AST.Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  let new = BasicBlock
        { blockName = AST.Name (BS.pack bname)
        , blockStack = []
        , blockTerm = Nothing
        }
  modify $ \s -> s { blocks = bls ++ [new], blockCount = ix + 1 }
  return (AST.Name (BS.pack bname))

setBlock :: AST.Name -> Codegen ()
setBlock bname = modify $ \s -> s { currentBlock = bname }

getBlock :: Codegen AST.Name
getBlock = gets currentBlock

modifyBlock :: (BasicBlock -> BasicBlock) -> Codegen ()
modifyBlock f = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = map update (blocks s) }
  where
    update block
      | blockName block == active = f block
      | otherwise = block

current :: Codegen BasicBlock
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case find (\x -> blockName x == c) blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

instr :: AST.Instruction -> Codegen AST.Operand
instr ins = do
  n <- fresh
  let ref = AST.LocalReference T.i32 n
  modifyBlock $ \b -> b { blockStack = blockStack b ++ [n := ins] }
  return ref

instr' :: T.Type -> AST.Instruction -> Codegen AST.Operand
instr' ty ins = do
  n <- fresh
  let ref = AST.LocalReference ty n
  modifyBlock $ \b -> b { blockStack = blockStack b ++ [n := ins] }
  return ref

terminator :: Named AST.Terminator -> Codegen ()
terminator term = modifyBlock $ \b -> b { blockTerm = Just term }

createBlocks :: CodegenState -> [AST.BasicBlock]
createBlocks s = map makeBlock (blocks s)
  where
    makeBlock (BasicBlock nm stk term) = AST.BasicBlock nm stk (makeTerm term)
    makeTerm (Just x) = x
    makeTerm Nothing = error $ "Block has no terminator"

-- | LLVM instructions
alloca :: T.Type -> Codegen AST.Operand
alloca ty = instr' (T.ptr ty) $ AST.Alloca ty Nothing 0 []

store :: AST.Operand -> AST.Operand -> Codegen ()
store ptr val = do
  instr $ AST.Store False ptr val Nothing 0 []
  return ()

load :: AST.Operand -> Codegen AST.Operand
load ptr = instr' T.i32 $ AST.Load False ptr Nothing 0 []

gep :: AST.Operand -> [AST.Operand] -> Codegen AST.Operand
gep addr indices = instr' (T.ptr T.i8) $ AST.GetElementPtr False addr indices []

add :: AST.Operand -> AST.Operand -> Codegen AST.Operand
add a b = instr $ AST.Add False False a b []

sub :: AST.Operand -> AST.Operand -> Codegen AST.Operand
sub a b = instr $ AST.Sub False False a b []

icmp :: IP.IntegerPredicate -> AST.Operand -> AST.Operand -> Codegen AST.Operand
icmp pred a b = instr' T.i1 $ AST.ICmp pred a b []

zext :: AST.Operand -> T.Type -> Codegen AST.Operand
zext val ty = instr' ty $ AST.ZExt val ty []

trunc :: AST.Operand -> T.Type -> Codegen AST.Operand
trunc val ty = instr' ty $ AST.Trunc val ty []

call :: AST.Operand -> [AST.Operand] -> Codegen AST.Operand
call fn args = instr $ AST.Call Nothing CC.C [] (Right fn) (map (\x -> (x, [])) args) [] []

ret :: AST.Operand -> Codegen ()
ret val = terminator $ AST.Do $ AST.Ret (Just val) []

br :: AST.Name -> Codegen ()
br target = terminator $ AST.Do $ AST.Br target []

condBr :: AST.Operand -> AST.Name -> AST.Name -> Codegen ()
condBr cond tr fl = terminator $ AST.Do $ AST.CondBr cond tr fl []

-- | Constants
iconst :: Integer -> Codegen AST.Operand
iconst n = return $ AST.ConstantOperand $ C.Int 32 n

iconst8 :: Integer -> Codegen AST.Operand
iconst8 n = return $ AST.ConstantOperand $ C.Int 8 n

externf :: String -> AST.Operand
externf name = AST.ConstantOperand $ C.GlobalReference
  (T.FunctionType T.i32 [] False)
  (AST.Name (BS.pack name))
