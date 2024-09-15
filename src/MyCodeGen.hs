module MyCodeGen where

import Sprockell
import qualified MyParser
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map


-- Lookup tables for local and global memory
type VarName = String
type PrintType = MyParser.MyType
type MemoryAddress = Int
type ThreadCount = Int

-- PrintType will be used only for printing (to indicate how the variable should be printed)
type LocalLookup = Map VarName (MemoryAddress, PrintType)
type GlobalLookup = Map VarName (MemoryAddress, PrintType)


-- represents the environment for a program, containing various state information
-- that will be used during code generation
-- run mainCode : threadsCode - runs the Spril instructions for all threads
data Env = Env { nextLocalAddr  :: MemoryAddress    -- local addresses: 36
               , nextGlobalAddr :: MemoryAddress    -- shared addresses: 8
               , localLookup    :: LocalLookup      -- lookup map for local memory
               , globalLookup   :: GlobalLookup     -- lookup map for shared memory
               , freeRegs       :: [RegAddr]        -- list of available registers
               , mainCode       :: [Instruction]    -- code for the main thread
               , threadsCode    :: [[Instruction]]  -- code for new threads
               , threadCounter  :: ThreadCount      -- counter used for creating names of signals
               } deriving (Show)

-- initial state of the environment for any program
initialEnv :: Env
initialEnv = Env { nextLocalAddr  = 0
                 , nextGlobalAddr = 0
                 , localLookup    = Map.empty
                 , globalLookup   = Map.empty
                 , freeRegs       = initRegs
                 , mainCode       = []
                 , threadsCode    = []
                 , threadCounter  = 0
                 }


-- add a local variable to the local lookup table
addLocalVariable :: Env -> PrintType -> VarName -> (MemoryAddress, Env)
addLocalVariable env typ name =
  let addr = nextLocalAddr env
      newLocalLookup = Map.insert name (addr, typ) (localLookup env)
      newEnv = env { nextLocalAddr = addr + 1, localLookup = newLocalLookup }
  in (addr, newEnv)


-- add a global variable to the global lookup table
addGlobalVariable :: Env -> PrintType -> VarName -> (MemoryAddress, Env)
addGlobalVariable env typ name =
  let addr = nextGlobalAddr env
      newGlobalLookup = Map.insert name (addr, typ) (globalLookup env)
      newEnv = env { nextGlobalAddr = addr + 1, globalLookup = newGlobalLookup }
  in (addr, newEnv)


--------------------------------------------------
--               CODE GENERATION                --
--------------------------------------------------

-- compile code for Program Block
compileProgram :: MyParser.Program -> Env
compileProgram (MyParser.Program programBlock) =
  let env = compileBlock initialEnv programBlock
  in env { mainCode = mainCode env ++ [EndProg] }


-- compile code for Statement
compileStmt :: Env -> MyParser.Statement -> Env
compileStmt env stmt = case stmt of
  -- call special compile functions
  MyParser.Declaration decl            -> compileDeclaration env decl
  MyParser.Assignment  asgn            -> compileAssignment  env asgn

  MyParser.Block       block           -> compileBlock  env block
  MyParser.Print       expr            -> compilePrint  env expr
  MyParser.Lock        name            -> compileLock   env name
  MyParser.Unlock      name            -> compileUnlock env name

  MyParser.If cond thenBlock elseBlock -> compileIf     env cond thenBlock elseBlock
  MyParser.While cond whileBlock       -> compileWhile  env cond whileBlock
  MyParser.Thread threadBlock          -> compileThread env threadBlock


-- compile code for Declaration
compileDeclaration :: Env -> MyParser.Declaration -> Env
compileDeclaration env decl = case decl of

  -- Primitive
  MyParser.Primitive scope typ name maybeExpr ->
    -- get addr and newEnv
    let (addr, newEnv) = if scope == MyParser.Global
                          then addGlobalVariable env typ name
                          else addLocalVariable  env typ name

        -- reg will be used to store the value of the variable (expr or default)
        reg = getTmpReg env
        declCode = case maybeExpr of

          -- evaluate expr and store in memory at addr
          Just expr ->  let exprCode = genExpr env expr reg
                        in if scope == MyParser.Global
                          then exprCode ++ [writeShMem reg addr]
                          else exprCode ++ [storeAddr reg addr]

          -- store default value depending on type at memory addr
          Nothing   -> case typ of
            -- default: 0
            MyParser.TInt   ->  let defaultCode = [loadI 0 reg]
                                in if scope == MyParser.Global
                                  then defaultCode ++ [writeShMem reg addr]
                                  else defaultCode ++ [storeAddr reg addr]

            -- default: 0 = False
            MyParser.TBool  ->  let defaultCode = [loadI 0 reg]
                                in if scope == MyParser.Global
                                  then defaultCode ++ [writeShMem reg addr]
                                  else defaultCode ++ [storeAddr reg addr]

            -- default: chr 32 = ' '; instead of 32 we can use (fromInteger $ ord ' ')
            MyParser.TChar  ->  let defaultCode = [loadI 32 reg]
                                in if scope == MyParser.Global
                                  then defaultCode ++ [writeShMem reg addr]
                                  else defaultCode ++ [storeAddr reg addr]

        -- update main code
        newMainCode = mainCode newEnv ++ declCode
    in newEnv { mainCode = newMainCode }

    -- TLock
  MyParser.TLock name ->
    -- locks are stored in shared memory
    -- locks are not allowed to be printed, but we still need a placeholder for PrintType
    let (addr, newEnv) = addGlobalVariable env MyParser.TInt name
        reg = getTmpReg env
        lockCode = [loadI 0 reg, storeAddr reg addr]
        newMainCode = mainCode newEnv ++ lockCode
    in newEnv { mainCode = newMainCode }

  -- Arrays and Strings are not supported
  MyParser.Array _ name _ _ -> error $ "Variable " ++ name ++ " is an array. Arrays are not supported!"
  MyParser.String name _    -> error $ "Variable " ++ name ++ " is a String. Strings are not suppoerted!"


-- compile code for Assignment
compileAssignment :: Env -> MyParser.Assignment -> Env
compileAssignment env asgn = case asgn of

  -- Absolute
  MyParser.Absolute name expr ->
    -- search for variable in local memory
    case Map.lookup name (localLookup env) of
      -- variable found in local lookup
      Just (addr, _) -> let reg = getTmpReg env
                            exprCode = genExpr env expr reg
                            storeCode = exprCode ++ [storeAddr reg addr]
                            newMainCode = mainCode env ++ storeCode
                        in env { mainCode = newMainCode }

      -- variable not found in local lookup
      Nothing   ->
        -- search for variable in shared memory
        case Map.lookup name (globalLookup env) of

          -- found
          Just (addr, _) -> let reg = getTmpReg env
                                exprCode = genExpr env expr reg
                                storeCode = exprCode ++ [writeShMem reg addr]
                                newMainCode = mainCode env ++ storeCode
                            in env { mainCode = newMainCode }

          -- not found in shared memory
          Nothing   -> error $ "Variable " ++ name ++ " not found! Are you sure you declared it?"

  -- Partial assignment is not supported
  MyParser.Partial name _ _ -> error $ "Partial assignment for variable " ++ name ++ " is not supported!"
  

-- compile code for Block
-- when leaving a block, we need to reset some values to their 
-- state before entering the block: nextLocalAddr, localLookup, freeRegs
-- remaining values are passed from within the block
compileBlock :: Env -> MyParser.Block -> Env
compileBlock env stmts =
    -- save states of env before entering block
    let oldLocalAddr    = nextLocalAddr env
        oldLocalLookup  = localLookup   env
        oldFreeRegs     = freeRegs      env

        -- compile the list of statements
        blockEnv = compileStatements env stmts

        -- restore states of env after exiting block
        newEnv = blockEnv { nextLocalAddr = oldLocalAddr
                          , localLookup   = oldLocalLookup
                          , freeRegs      = oldFreeRegs
                          }
    -- return new env
    in newEnv


-- compile all statements and pass the new env between them
compileStatements :: Env -> [MyParser.Statement] -> Env
compileStatements = foldl compileStmt


-- compile code for Print
compilePrint :: Env -> MyParser.Expr -> Env

-- special case to print String literals
compilePrint env (MyParser.StringLiteral str) =
  let reg = getTmpReg env
      printCode = printLine str reg
      newMainCode = mainCode env ++ printCode

  -- update mainCode
  in env { mainCode = newMainCode }

-- handle other types
compilePrint env expr =
  -- reg1 holds value of expr to be printed
  let (reg1, newEnv) = getReg env
      reg2           = getTmpReg newEnv
      exprType       = exprPrintType env expr
      exprCode       = genExpr env expr reg1    -- pass env here -> use freeRegs of env

      printCode = case exprType of
        MyParser.TInt  -> [printConst reg1]

        MyParser.TChar -> printSource reg2
                          ++ [printChar reg1]
                          ++ printN reg2

        MyParser.TBool -> let fCode = printLine "false" reg2
                              tCode = printLine "true" reg2
                              offset1 = length fCode + 2
                              offset2 = length tCode + 1
                          in [branchRel reg1 offset1]
                             ++ fCode
                             ++ [jumpRel offset2]
                             ++ tCode
      
      newMainCode = mainCode env ++ exprCode ++ printCode
  in env { mainCode = newMainCode }


-- get type of expr for printing (TInt or TChar; TBool is printed as TInt)
exprPrintType :: Env -> MyParser.Expr -> MyParser.MyType
exprPrintType _   (MyParser.Condition _) = MyParser.TBool
exprPrintType _   (MyParser.Char _) = MyParser.TChar
exprPrintType env (MyParser.Var name) =

  -- search in local memory
  case Map.lookup name (localLookup env) of
    -- Just (_, typ) -> if typ == MyParser.TChar then typ else MyParser.TInt
    Just (_, typ) -> typ
    Nothing       ->

      -- search in shMem
      case Map.lookup name (globalLookup env) of
        Just (_, typ) -> typ
        Nothing       -> error $ "Variable " ++ name ++ " not found! Did you declare it?"

-- all other expr constructors
exprPrintType _ _ = MyParser.TInt


-- compile code for Lock
compileLock :: Env -> MyParser.VarName -> Env
compileLock env name =

    -- searcch for lock in globalLookup
    case Map.lookup name (globalLookup env) of
      -- lock found
      Just (addr, _) -> let lockCode    = getLock  env addr
                            newMainCode = mainCode env ++ lockCode
                        in env { mainCode = newMainCode }
      -- lock not found
      Nothing    -> error $ "Lock " ++ name ++ " not found! Are you sure you declared it?"


-- compile code for Unlock (similar to Lock)
compileUnlock :: Env -> MyParser.VarName -> Env
compileUnlock env name =

    -- search for lock in globalLookup
    case Map.lookup name (globalLookup env) of
      -- found
      Just (addr, _) -> let unlockCode  = releaseLock addr
                            newMainCode = mainCode env ++ unlockCode
                        in env { mainCode = newMainCode }
      -- not found
      Nothing    -> error $ "Lock " ++ name ++ " not found! Are you sure you declared it?"


-- compile code for If
compileIf :: Env -> MyParser.Condition -> MyParser.Block -> Maybe MyParser.Block -> Env
compileIf env cond thenBlock maybeElseBlock =

  -- generate not cond for conditional branch
  -- if reg false => do then, jump after else
  -- if reg true  => conditional branch to else
  let (reg, newEnv) = getReg env
      condCode      = genNotCond newEnv cond reg

      -- save current state of mainCode
      oldMainCode = mainCode env

      -- compile thenBlock with current env
      thenEnv = compileBlock env thenBlock

      -- calculate length of then block (with old mainBlock)
      thenLength = length (mainCode thenEnv)

      -- handle maybe elseBlock
      (elseEnv, elseLength, jumpAfterElse) = case maybeElseBlock of

        -- else exists
        Just elseBlock -> let compiledElseEnv = compileBlock thenEnv elseBlock
                              elseLength = length (mainCode compiledElseEnv) - thenLength
                          in (compiledElseEnv, elseLength, elseLength + 1)

        -- else does not exist => jumpAfterElse is 1 (or we can use 0)
        Nothing -> (thenEnv, 0, 1)

      -- generate relative jumps
      -- thenLength + 1 (last instr of then) + 1 (branch after then) 
      jumpToElse = thenLength - length oldMainCode + 2

      -- generate if code
      -- depending on elseLength append fitting else instructions
      ifCode = condCode
               ++ [branchRel reg jumpToElse]
               ++ drop (length oldMainCode) (mainCode thenEnv)
               ++ [jumpRel jumpAfterElse]
               ++ (if elseLength > 0 then drop thenLength (mainCode elseEnv) else [])

      -- add ifCode to old mainCode
      newMainCode = oldMainCode ++ ifCode

  -- return elseEnv with updated mainCode
  in elseEnv { mainCode = newMainCode }


-- compile code for While
compileWhile :: Env -> MyParser.Condition -> MyParser.Block -> Env
compileWhile env cond whileBlock =

  -- structure: loop code ++ condition code

  -- generate cond for conditional branch
  -- if reg false => continue
  -- if reg true  => jump to start of loop
  let (reg, newEnv) = getReg env
      condCode      = genCond newEnv cond reg

      -- save current state of mainCode
      oldMainCode = mainCode env

      -- generate code for while block with current env
      whileEnv = compileBlock env whileBlock

      -- calculate length of whileEnv (with old mainCode)
      whileLength = length (mainCode whileEnv) - length oldMainCode
      -- calculate length of condCode
      condLength  = length condCode

      -- calculate relative jumps
      -- jumpt to len (while - main) + 1 (last instr of while)
      jumpToCond  = whileLength + 1
      -- jumpToWhile = len cond + len while
      jumpToWhile = condLength + whileLength

      -- generate while code
      whileCode = [jumpRel jumpToCond]
                  ++ drop (length oldMainCode) (mainCode whileEnv)
                  ++ condCode
                  ++ [branchRel reg (-jumpToWhile)]

      -- update old mainCode
      newMainCode = oldMainCode ++ whileCode

  -- return whileEnv with new mainCode
  in whileEnv { mainCode = newMainCode }


-- compile code for Thread {Block}
compileThread :: Env -> MyParser.Block -> Env
compileThread env threadBlock =

  -- create unique signal variable and update counter
  let tCount = threadCounter env
      signalName = "threadSignal" ++ show tCount
      updatedEnv = env { threadCounter = tCount + 1 }

      -- add signal to shared mem
      -- signal is not allowed to be printed, but we need a placeholder for PrintType
      (signalAddr, signalEnv) = addGlobalVariable updatedEnv MyParser.TInt signalName

      -- reset env for thread {block}
      threadEnv = initialEnv { nextGlobalAddr = nextGlobalAddr signalEnv
                             , globalLookup   = globalLookup   signalEnv
                             }

      -- compile thread {block}
      compiledThreadEnv = compileBlock threadEnv threadBlock

      -- create "jail" code for the thread
      -- getTmpReg from compiled threadEnv because each thread has separate regs
      -- signalReg can be 0 or 1 (because we will use testAndSet)
      -- this reg is temp because once the thread leaves his "jail" the reg becomes available again
      signalReg = getTmpReg compiledThreadEnv
      jailCode  = readShMem signalAddr signalReg  -- 2 instructions
                  ++ [flipReg signalReg]          -- 1 instruction
                  ++ [branchRel signalReg (-3)]

      -- add "jail" code to the mainCode of compiledThreadEnv
      tCode = jailCode ++ mainCode compiledThreadEnv ++ [EndProg]

      -- update threadsCode in env (or updatedEnv)
      -- newThreadsCode = tCode : threadsCode env
      newThreadsCode = threadsCode env ++ [tCode] ++ threadsCode compiledThreadEnv

      -- create code to testAndSet the signal to 1 in shMem
      -- we will not read reg but we still need to receive from TestAndSet
      startSignalCode = let reg = getTmpReg updatedEnv
                        in testAndSet signalAddr reg

      -- update main code (we can use env or updatedEnv)
      newMainCode = mainCode env ++ startSignalCode

      -- update env with data from compiledThreadEnv
      newEnv = updatedEnv { nextGlobalAddr = nextGlobalAddr compiledThreadEnv
                          , globalLookup   = globalLookup compiledThreadEnv
                          , mainCode       = newMainCode
                          , threadsCode    = newThreadsCode
                          }
  -- return new env
  in newEnv


-- generate Expr
-- evaluates expr and stores result in reg
genExpr :: Env -> MyParser.Expr -> RegAddr -> [Instruction]
genExpr env expr reg = case expr of

  -- constant, char, variable, condition
  MyParser.Const     val  -> [loadI val reg]
  MyParser.Char      val  -> [loadI (toInteger $ ord val) reg]
  MyParser.Var       name -> loadVar env name reg
  MyParser.Condition cond -> genCond env cond reg

  -- binary operations
  MyParser.Add  e1 e2     -> genBinExpr env Add e1 e2 reg
  MyParser.Mult e1 e2     -> genBinExpr env Mul e1 e2 reg
  MyParser.Sub  e1 e2     -> genBinExpr env Sub e1 e2 reg
  MyParser.Div  e1 e2     -> genDiv env e1 e2 reg

  -- Arrays and Strings are not supported!
  MyParser.ArrayLiteral _  -> error "Arrays are not supported!"
  MyParser.ArrayIndex _ _  -> error "Arrays are not supported!"
  MyParser.StringLiteral _ -> error "Strings are not supported!"


-- generate code for "soft" (integer) division (N, D)
-- divide N / D aka e1 / e2
genDiv :: Env -> MyParser.Expr -> MyParser.Expr -> RegAddr -> [Instruction]
genDiv env n d reg =
  -- genereate code for exprs
  let dCode = genExpr env d reg ++ [Push reg]  -- push denominator on stack
      nCode = genExpr env n reg

      -- use 5 regs
      env1 = occupyReg env reg
      regN = reg
      (regD, env2) = getReg env1
      (regQ, env3) = getReg env2
      (regR, env4) = getReg env3
      regTmp       = getTmpReg env4

      -- regQ = regN / regD
      -- stack will be used to store 2 booleans: negQ, checkR
      divCode = [
        -- check for division by zero
          Pop regD                        -- regD = D
        , loadI 0 regQ                    -- regQ = 0
        , Compute Equal regD reg0 regTmp  -- regTmp = (regD == 0)
        , branchRel regTmp 30             -- if regD == 0, branch to end (skip div)

        -- if D < 0 then (Q, R) := divide(N, −D); return (−Q, R) end
        , Push reg0                       -- push negQ = 0 on stack
        , Compute GtE regD reg0 regTmp
        , branchRel regTmp 5              -- if regD >= 0, branch to next check (N < 0)
        , negateReg regD                  -- regD = -regD
        , loadI 1 regTmp
        , Pop reg0                        -- pop negQ = 0 from stack    
        , Push regTmp                     -- push negQ = 1 to stack

        -- if N < 0 then
        , Push reg0                       -- push checkR = 0 on stack
        , Compute GtE regN reg0 regTmp
        , branchRel regTmp 5              -- if regN >= 0, branch to unsigned division
        -- (Q,R) := divide(−N, D)
        , negateReg regN                  -- regN = -regN
        , loadI 1 regTmp
        , Pop reg0                        -- pop checkR = 0 from stack
        , Push regTmp                     -- push checkR = 1 on stack

        -- start unsigned division (modifies: regQ, reqR)
        , copyReg regN regR               -- regR = regN
        -- loop
        , Compute Lt regR regD regTmp 
        , branchRel regTmp 5              -- if regR < regD, break loop
        , loadI 1 regTmp                
        , Compute Add regQ regTmp regQ    -- regQ += 1
        , Compute Sub regR regD regR      -- regR -= regD
        , jumpRel (-5)                    -- repeat loop

        -- checkR
        , Pop regTmp                      -- pop checkR
        , flipReg regTmp                  -- flip checkR for branch
        , branchRel regTmp 2              -- if checkR == 0, branch to negQ
        -- if R = 0 then return (−Q, 0) else return (−Q − 1, D − R) 
        , negateReg regQ                  -- regQ = -regQ

        -- negQ
        , Pop regTmp                      -- pop negQ
        , flipReg regTmp                  -- flip negQ for branch
        , branchRel regTmp 2              -- if negQ == 0, branch to end
        , negateReg regQ

        -- copy result to reg
        , copyReg regQ reg
        ]
  -- combine code
  -- it is important to keep this order (because we store d on stack)!
  in dCode ++ nCode ++ divCode


-- generate Condition
-- evaluates cond and stores result in reg
genCond :: Env -> MyParser.Condition -> RegAddr -> [Instruction]
genCond env cond reg = case cond of

  -- binary operations
  MyParser.Eq  c1 c2 -> genBinCond env Equal c1 c2 reg
  MyParser.Neq c1 c2 -> genBinCond env NEq   c1 c2 reg
  MyParser.Gt  c1 c2 -> genBinCond env Gt    c1 c2 reg
  MyParser.Lt  c1 c2 -> genBinCond env Lt    c1 c2 reg
  MyParser.Ge  c1 c2 -> genBinCond env GtE   c1 c2 reg
  MyParser.Le  c1 c2 -> genBinCond env LtE   c1 c2 reg
  MyParser.And c1 c2 -> genBinCond env And   c1 c2 reg
  MyParser.Or  c1 c2 -> genBinCond env Or    c1 c2 reg

  -- unary operation (Not), Boolean, Expr
  MyParser.Not     c -> genNotCond  env c reg
  MyParser.Boolean b -> genBoolCond env b reg
  MyParser.Expr    e -> genExpr     env e reg

-- generate code for Expr binary operations (except division):
-- Add, Mult, Sub
genBinExpr :: Env -> Operator -> MyParser.Expr -> MyParser.Expr -> RegAddr -> [Instruction]
genBinExpr env op e1 e2 reg1 =
  -- occupy reg1 so that reg1 and reg2 are different
  -- use newEnv if you want to use list of freeRegs withou reg1
  -- use env if you want to use list of freeRegs with reg1
  let newEnv = occupyReg env reg1
      reg2 = getTmpReg newEnv
  in genExpr env e1 reg1
  ++ [Push reg1]
  ++ genExpr env e2 reg1
  ++ [Pop reg2]
  ++ [Compute op reg2 reg1 reg1]


-- generate code for Condition binary operations:
-- Eq, Neq, Gt, Lt, Ge, Le, And, Or
genBinCond :: Env -> Operator -> MyParser.Condition -> MyParser.Condition -> RegAddr -> [Instruction]
genBinCond env op c1 c2 reg1 =
  let newEnv = occupyReg env reg1
      reg2 = getTmpReg newEnv
  in genCond env c1 reg1
  ++ [Push reg1]
  ++ genCond env c2 reg1
  ++ [Pop reg2]
  ++ [Compute op reg2 reg1 reg1]

-- generate negation of a condition
-- condition can be 1 or 0 => check if reg0 == cond:
-- 0 == 0 => 1;  0 == 1 => 0
genNotCond :: Env -> MyParser.Condition -> RegAddr -> [Instruction]
genNotCond env cond reg =
     genCond env cond reg
  ++ [flipReg reg]

-- generate boolean
genBoolCond :: Env -> Bool -> RegAddr -> [Instruction]
genBoolCond env bool reg = [loadI (toInteger $ fromEnum bool) reg]


------------------------------------------------------
--               SPROCKELL EXTENSIONS               --
------------------------------------------------------

------------------------------
--     Manage Registers     --
------------------------------

-- initial state of free registers
initRegs :: [RegAddr]
initRegs = [regA, regB, regC, regD, regE, regF]

-- get a free register and remove it from the list of available registers
getReg :: Env -> (RegAddr, Env)
getReg env = case freeRegs env of
  []       -> error "No free registers!"
  (r:rs)   -> (r, env { freeRegs = rs })

-- occupy a register
occupyReg :: Env -> RegAddr -> Env
occupyReg env reg =
  let newFreeRegs = filter (/= reg) (freeRegs env)
  in env { freeRegs = newFreeRegs }

-- release a register -> adds it to the list of available registers
releaseReg :: RegAddr -> Env -> Env
releaseReg reg env = env { freeRegs = reg : freeRegs env }

-- sometimes we need to get and release a register in the same "block" of instructions
-- this allowes for easier use of temporary registers
getTmpReg :: Env -> RegAddr
getTmpReg env = case freeRegs env of
  []          -> error "No free registers!"
  (r:_)       -> r

-- flip the value of a reg (0 -> 1; 1 -> 0)
-- assume reg contains 0 or 1
flipReg :: RegAddr -> Instruction
flipReg reg = Compute Equal reg0 reg reg

-- negate the value of a reg
negateReg :: RegAddr -> Instruction
negateReg reg = Compute Sub reg0 reg reg


-------------------------------
--     Load in Registers     --
-------------------------------

-- memory address offset
type Offset = Integer

-- load an immediate value to a register
loadI :: Integer -> RegAddr -> Instruction
loadI val = Load (ImmValue $ fromInteger val)

-- load value from address contained in reg1 to reg2 
load :: RegAddr -> RegAddr -> Instruction
load reg1 = Load (IndAddr reg1)

-- load value from MemAddr into reg
loadAddr :: MemAddr -> RegAddr -> Instruction
loadAddr addr = Load (DirAddr addr)

-- load after an immediate value to reg3
-- loadAI env addr offset target
loadAI :: Env -> RegAddr -> Offset -> RegAddr -> [Instruction]
loadAI env reg1 offset reg3 =
  let reg2 = getTmpReg env
  in [loadI offset reg2]
  ++ [Compute Add reg1 reg2 reg2]
  ++ [Load (IndAddr reg2) reg3]

-- load a primitive variable from memory to reg
loadVar :: Env -> VarName -> RegAddr -> [Instruction]
loadVar env name reg = case Map.lookup name (localLookup env) of
  -- load from local memory
  Just (addr, _) -> [loadAddr addr reg]
  Nothing        -> case Map.lookup name (globalLookup env) of
    -- load from shMem
    Just (addr, _) -> readShMem addr reg
    Nothing        -> error $ "Variable " ++ name ++ " not found! Are you sure you defined it?"

-- copy a value from reg1 to reg2
copyReg :: RegAddr -> RegAddr -> Instruction
copyReg = Compute Add reg0

-- stores value from reg to MemAddr
storeAddr :: RegAddr -> MemAddr -> Instruction
storeAddr reg addr = Store reg (DirAddr addr)

-- stores value from reg to addr plus offset
storeAI :: Env -> RegAddr -> RegAddr -> Offset -> [Instruction]
storeAI env reg1 reg2 offset =
  let reg3 = getTmpReg env
  in [loadI offset reg3]
  ++ [Compute Add reg2 reg3 reg3]
  ++ [Store reg1 (IndAddr reg3)]


---------------------------
--     Shared Memory     --
---------------------------

-- request to read shMem addr and receive result in reg
readShMem :: MemAddr -> RegAddr -> [Instruction]
readShMem addr reg = [ReadInstr (DirAddr addr), Receive reg]

-- write content of reg to shMem addr
writeShMem :: RegAddr -> MemAddr -> Instruction
writeShMem reg addr = WriteInstr reg (DirAddr addr)

-- try to acquire the lock for MemAddr
-- reg is used to check if lock is free or not
getLock :: Env -> MemAddr -> [Instruction]
getLock env addr =
  let reg = getTmpReg env
  in testAndSet addr reg
  ++ [flipReg reg]
  ++ [branchRel reg (-3)]

-- releases a lock -> write 0 at shared MemAddr
releaseLock :: MemAddr -> [Instruction]
releaseLock addr = [writeShMem reg0 addr]

-- TestAndSet from 0 to 1 in shMem and receive response in reg: 1 - success; 0 - failure
testAndSet :: MemAddr -> RegAddr -> [Instruction]
testAndSet addr reg = TestAndSet (DirAddr addr) : [Receive reg]


---------------------------
--     Jump / Branch     --
---------------------------

-- jump relative
jumpRel :: CodeAddr -> Instruction
jumpRel addr = Jump (Rel addr)

-- branch relative if reg != 0
branchRel :: RegAddr -> CodeAddr -> Instruction
branchRel reg addr = Branch reg (Rel addr)


----------------------
--     Printing     --
----------------------

-- print a number from reg into numberIO
printConst :: RegAddr -> Instruction
printConst reg = WriteInstr reg numberIO

-- print a character from reg
printChar :: RegAddr -> Instruction
printChar reg = WriteInstr reg charIO

-- print a char literal
-- reg will be used to store the char and print it
printCharLit :: Char -> RegAddr -> [Instruction]
printCharLit c reg =
  loadI (toInteger $ ord c) reg : [printChar reg]

-- print a string literal
-- reg will be used for storing each char of string literal and printing it
printStrLit :: String -> RegAddr -> [Instruction]
printStrLit str reg = concatMap (`printCharLit` reg) str

-- prints a new line (ord '\n' = 10)
-- reg will be used to store value of '\n' and print it
printN :: RegAddr -> [Instruction]
printN = printCharLit '\n'

-- prints "Sprockell n says "
-- reg is used for intermediate calculations
printSource :: RegAddr -> [Instruction]
printSource reg =
  printStrLit "Sprockell " reg
  ++ printDigChar regSprID reg
  ++ printStrLit " says " reg

-- prints a digit from reg1 as a char to charIO; reg2 is used for calculations
printDigChar :: RegAddr -> RegAddr -> [Instruction]
printDigChar reg1 reg2 =
  [loadI (toInteger $ ord '0') reg2]
  ++ [Compute Add reg1 reg2 reg2]
  ++ [printChar reg2]

-- prints a string literal with source (sprockell id) and new line
printLine :: String -> RegAddr -> [Instruction]
printLine str reg =
  printSource reg
  ++ printStrLit str reg
  ++ printN reg