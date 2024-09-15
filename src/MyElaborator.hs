module MyElaborator where

import MyParser
import Text.Parsec
import Data.Either
import Data.Maybe
import Data.List


------------------------------------------------
--                   ERROR                    --
------------------------------------------------

-- Data type for error returning from the frontend
data CompileError = ParseError ParseError | TypeError String deriving (Show)

-- gets the string representation of the data type of the variables for errors
getStringType :: HashmapType -> String
getStringType HInt = "Int"
getStringType HBool = "Bool"
getStringType HChar = "Char"
getStringType HLock = "Lock"
getStringType HString = "String"
getStringType (HArray HInt) = "Int[]"
getStringType (HArray HBool) = "Bool[]"
getStringType (HArray HChar) = "Char[]"

-- Helper function to return the appropriate error message when a variable has a different data type than what was expected
errorExpected :: HashmapType -> String
errorExpected varType = "Unexpected data type " ++ getStringType varType ++ " in expression!"



------------------------------------------------
--          FRONTEND CODE ANALYSIS            --
------------------------------------------------

-- parse program "" input -> parse from input
-- typeCheckProgram prog  -> perform type check on AST and return new AST

-- create AST from IO for testing purposes
createASTIO :: FilePath -> IO (Either CompileError Program)
createASTIO filePath = do
    input <- readFile filePath
    let res = parse program "" input
    return $ case res of
        Left err -> Left (ParseError err)
        Right prog -> case typeCheckProgram prog of
            Left typeErr -> Left (TypeError typeErr)
            Right checkedProg -> Right checkedProg

-- parse input -> generate ast -> perform elaboration
createAST :: String -> Either CompileError Program
createAST input = case parseResult of
                       Left err     -> Left (ParseError err)
                       Right parsed -> case typeCheckProgram parsed of
                                            Left err  -> Left (TypeError err)
                                            Right ast -> Right ast
   where
      parseResult = parse program "" input


------------------------------------------------
--                ELABORATION                 --
------------------------------------------------

-- Performs type check on the program returning either a string, which represents an error, 
-- or a program(in case of variable shadowing do renaming and return the renamed program) 
typeCheckProgram :: Program -> Either String Program
typeCheckProgram (Program block)
   | isLeft either1 = Left (fromLeft "" either1)
   | isLeft either2 = Left (fromLeft "" either2)
   | otherwise = Right (Program (fromRight [] either2))
   where
      either1 = checkReDeclaration block []
      either2 = typeCheckingBlock block (Scope [] []) []


-- Data type for creating either a hashmap or a stack, used for type checking a program
data HashmapType = HInt | HBool | HChar | HLock | HString | HArray HashmapType  deriving (Show, Eq)
data Hashmap = Scope [(HashmapType, VarName)] [Hashmap] deriving (Show)

-- The function extracts from the hashmap the data type of the variable with certain name
-- In case inside the hashmap there is not the variable, then return Left message
extractTypeHashmap :: Hashmap -> VarName -> Either String HashmapType
extractTypeHashmap (Scope list []) s
   | s `elem` map snd list = Right (fst $ fromJust (find (\x -> snd x == s) list))
   | otherwise = Left ("The variable has not been declared yet: " ++ s)
extractTypeHashmap (Scope list [h]) s
   | s `elem` map snd list = Right (fst $ fromJust (find (\x -> snd x == s) list))
   | otherwise = extractTypeHashmap h s


-- Creates a new empty scope at the top of the hashmap
newBlockHashmap :: Hashmap -> Hashmap
newBlockHashmap (Scope list []) = Scope list [Scope [] []]
newBlockHashmap (Scope list [s]) = Scope list [newBlockHashmap s]


-- -- Inserts in the current scope the variable and its type
insertHashmap :: (HashmapType, String) -> Hashmap -> Hashmap
insertHashmap v (Scope list []) = Scope (list ++ [v]) []
insertHashmap v (Scope list [s]) = Scope list [insertHashmap v s]


-- Changes the data type MyType to HashmapType
changeType :: MyType -> HashmapType
changeType TInt = HInt
changeType TChar = HChar
changeType TBool = HBool


-- Checks if the array declaration is correct, checking the size at the same, otherwise return the error
checkArraySize :: Declaration -> Either String Bool
checkArraySize (Array _ _ size (Just (ArrayLiteral array)))
   | size == 0 = Left "Cannot give array size 0"
   | toInteger actualSize == size = Right True
   | otherwise = Left ("Difference between the size of the array " ++ show size ++ ", and the number of the elements given " ++ show actualSize)
   where
      actualSize = length array
checkArraySize (Array _ _ size _)
   | size == 0 = Left "Cannot give array size 0"
   | otherwise = Right True
checkArraySize _ = Right True



-- The function checks whever inside the thread block were declared things that should not have been declared.
-- In case inside the block were declared thing that should not have been declared then return an error
checkThread :: [Statement] -> Either String Bool
checkThread [] = Right True
checkThread ((Declaration (TLock _)):xs) = Left "Cannot declare Lock inside a thread block"
checkThread ((Declaration (Primitive Global _ _ _)):xs) = Left "Cannot declare global variables inside a thread block"
checkThread ((If _ block mayBlock):xs)
   | isJust mayBlock = case x of
                        Left err -> Left err
                        Right prog -> checkThread (fromJust mayBlock)
   | otherwise = x
   where
      x = case checkThread block of
         Left err -> Left err
         Right prog -> checkThread xs
checkThread ((While _ block):xs) = case checkThread block of
                                    Left err -> Left err
                                    Right prog -> checkThread xs
checkThread ((Block block):xs) = case checkThread block of
                                    Left err -> Left err
                                    Right prog -> checkThread xs
checkThread (_:xs) = checkThread xs


-- The function checks whever inside the while block were declared things that should not have been declared.
-- In case inside the block were declared thing that should not have been declaredm then return an error
checkWhile :: [Statement] -> Either String Bool
checkWhile [] = Right True
checkWhile ((Declaration (TLock _):xs)) = Left "Cannot declare Lock inside a while block"
checkWhile ((Declaration (Primitive Global _ _ _)):xs) = Left "Cannot declare global variables inside while block"
checkWhile ((Thread block):xs) = Left "Cannot declare threads inside while block"
checkWhile ((If _ block mayBlock):xs)
   | isJust mayBlock = case x of
                        Left err -> Left err
                        Right prog -> checkWhile (fromJust mayBlock)
   | otherwise = x
   where
      x = case checkWhile block of
         Left err -> Left err
         Right prog -> checkWhile xs
checkWhile ((Block block):xs) = case checkWhile block of
                                 Left err -> Left err
                                 Right prog -> checkWhile xs
checkWhile ((While _ block):xs) = case checkWhile block of
                                    Left err -> Left err
                                    Right prog -> checkWhile xs
checkWhile (_:xs) = checkWhile xs


-- Checks if the string declaration is correct, otherwise return the error
checkStringDecl :: Declaration -> Either String Bool
checkStringDecl (String name (StringLiteral _)) = Right True
checkStringDecl (String _ _) = Left "Did not declare the string correctly"
checkStringDecl _ = Right True


-- The function checks there are redeclaration of the same variable within the same scope
-- If there are, then return Left error
checkReDeclaration :: [Statement] -> [String] -> Either String Bool
checkReDeclaration [] _ = Right True
checkReDeclaration ((Declaration (Primitive _ _ name _)):xs) list
   | name `elem` list = Left ("Variable " ++ name ++ " was declared twice")
   | otherwise = checkReDeclaration xs (list ++ [name])
checkReDeclaration ((Declaration (TLock name)):xs) list
   | name `elem` list = Left ("Variable " ++ name ++ " was declared twice")
   | otherwise = checkReDeclaration xs (list ++ [name])
checkReDeclaration ((Declaration (Array _ name _ _)):xs) list
   | name `elem` list = Left ("Variable " ++ name ++ " was declared twice")
   | otherwise = checkReDeclaration xs (list ++ [name])
checkReDeclaration ((Declaration (String name _)):xs) list
   | name `elem` list = Left ("Variable " ++ name ++ " was declared twice")
   | otherwise = checkReDeclaration xs (list ++ [name])
checkReDeclaration ((Thread block):xs) list = case checkReDeclaration block [] of
   Left err -> Left err
   Right prog -> checkReDeclaration xs list
checkReDeclaration ((Block block):xs) list = case checkReDeclaration block [] of
   Left err -> Left err
   Right prog -> checkReDeclaration xs list
checkReDeclaration ((If cond block mayBlock):xs) list
   | isJust mayBlock = case x of
                           Left err -> Left err
                           Right prog -> checkReDeclaration (fromJust mayBlock) []
   | otherwise = x
   where
      x = case checkReDeclaration block [] of
         Left err -> Left err
         Right prog -> checkReDeclaration xs list
checkReDeclaration ((While cond block):xs) list = case checkReDeclaration block [] of
         Left err -> Left err
         Right prog -> checkReDeclaration xs list
checkReDeclaration (_:xs) list = checkReDeclaration xs list


-- This is the type checking function for expressions that checks if the expressions inside it are logically correct and if it is correct then return the data type it produces
-- Otherwise if stumbles upon some errors, then return that error
typeCheckingExpr :: Expr -> [HashmapType] -> Hashmap -> Either String HashmapType
typeCheckingExpr (Const _) list hashmap
   | HInt `elem` list = Right HInt
   | otherwise = Left (errorExpected HInt)
typeCheckingExpr (Char _) list hashmap
   | HChar `elem` list = Right HChar
   | otherwise = Left (errorExpected HChar)
typeCheckingExpr (Var x) list hashmap
   | isLeft eitherVarType = eitherVarType
   | fromRight HInt eitherVarType `elem` list = eitherVarType
   | otherwise = Left (errorExpected (fromRight HInt eitherVarType))
   where
      eitherVarType = extractTypeHashmap hashmap x

typeCheckingExpr (Add expr1 expr2) list hashmap
   | isLeft eitherVarType1 = eitherVarType1
   | isLeft eitherVarType2 = eitherVarType2
   | HInt `elem` list = Right HInt
   | otherwise = Left (errorExpected HInt)
   where
      eitherVarType1 = typeCheckingExpr expr1 [HInt] hashmap
      eitherVarType2 = typeCheckingExpr expr2 [HInt] hashmap

typeCheckingExpr (Sub expr1 expr2) list hashmap
   | isLeft eitherVarType1 = eitherVarType1
   | isLeft eitherVarType2 = eitherVarType2
   | HInt `elem` list = Right HInt
   | otherwise = Left (errorExpected HInt)
   where
      eitherVarType1 = typeCheckingExpr expr1 [HInt] hashmap
      eitherVarType2 = typeCheckingExpr expr2 [HInt] hashmap

typeCheckingExpr (Mult expr1 expr2) list hashmap
   | isLeft eitherVarType1 = eitherVarType1
   | isLeft eitherVarType2 = eitherVarType2
   | HInt `elem` list = Right HInt
   | otherwise = Left (errorExpected HInt)
   where
      eitherVarType1 = typeCheckingExpr expr1 [HInt] hashmap
      eitherVarType2 = typeCheckingExpr expr2 [HInt] hashmap

typeCheckingExpr (Div expr1 expr2) list hashmap
   | isLeft eitherVarType1 = eitherVarType1
   | isLeft eitherVarType2 = eitherVarType2
   | HInt `elem` list = Right HInt
   | otherwise = Left (errorExpected HInt)
   where
      eitherVarType1 = typeCheckingExpr expr1 [HInt] hashmap
      eitherVarType2 = typeCheckingExpr expr2 [HInt] hashmap

typeCheckingExpr (Condition cond) list hashmap
   | isLeft eitherVarType = eitherVarType
   | HBool `elem` list = Right HBool
   | otherwise = Left (errorExpected HBool)
   where
      eitherVarType = typeCheckingCond cond [HBool] hashmap

typeCheckingExpr (ArrayLiteral exprList) list hashmap
   | null exprList = Left "The array cannot be empty"
   | otherwise = typeCheckingListExpr exprList list hashmap

typeCheckingExpr (ArrayIndex x expr) list hashmap
   | isLeft eitherVarType = eitherVarType
   | varType `elem` list = Right varType
   | otherwise = Left (errorExpected varType)
   where
      varType = fromHArray (fromRight HInt eitherVarType)
      eitherVarType = extractTypeHashmap hashmap x

typeCheckingExpr (StringLiteral _) list hashmap
   | HString `elem` list = Right HString
   | otherwise = Left (errorExpected HString)


-- This is the type checking function for conditions that checks if the expressions inside it are logically correct and if it is correct then return HBool
-- Otherwise if stumbles upon some errors, then return that error
typeCheckingCond :: Condition -> [HashmapType] -> Hashmap -> Either String HashmapType
typeCheckingCond (Eq cond1 cond2) list hashmap
   | isLeft cond1Type = cond1Type
   | isLeft cond2Type = cond2Type
   | cond1Type /= cond2Type = Left "Comparing different data types"
   | HBool `elem` list = Right HBool
   | otherwise = Left (errorExpected HBool)
   where
      cond1Type = typeCheckingCond cond1 [HInt, HBool, HChar, HArray HInt, HArray HChar, HArray HBool, HString] hashmap
      cond2Type = typeCheckingCond cond2 [HInt, HBool, HChar, HArray HInt, HArray HChar, HArray HBool, HString] hashmap

typeCheckingCond (Neq cond1 cond2) list hashmap
   | isLeft cond1Type = cond1Type
   | isLeft cond2Type = cond2Type
   | cond1Type /= cond2Type = Left "Comparing different data types"
   | HBool `elem` list = Right HBool
   | otherwise = Left (errorExpected HBool)
   where
      cond1Type = typeCheckingCond cond1 [HInt, HBool, HChar, HArray HInt, HArray HChar, HArray HBool, HString] hashmap
      cond2Type = typeCheckingCond cond2 [HInt, HBool, HChar, HArray HInt, HArray HChar, HArray HBool, HString] hashmap

typeCheckingCond (Gt cond1 cond2) list hashmap
   | isLeft cond1Type = cond1Type
   | isLeft cond2Type = cond2Type
   | cond1Type /= cond2Type = Left "Comparing different data types"
   | HBool `elem` list = Right HBool
   | otherwise = Left (errorExpected HBool)
   where
      cond1Type = typeCheckingCond cond1 [HInt, HChar] hashmap
      cond2Type = typeCheckingCond cond2 [HInt, HChar] hashmap

typeCheckingCond (Lt cond1 cond2) list hashmap
   | isLeft cond1Type = cond1Type
   | isLeft cond2Type = cond2Type
   | cond1Type /= cond2Type = Left "Comparing different data types"
   | HBool `elem` list = Right HBool
   | otherwise = Left (errorExpected HBool)
   where
      cond1Type = typeCheckingCond cond1 [HInt, HChar] hashmap
      cond2Type = typeCheckingCond cond2 [HInt, HChar] hashmap

typeCheckingCond (Ge cond1 cond2) list hashmap
   | isLeft cond1Type = cond1Type
   | isLeft cond2Type = cond2Type
   | cond1Type /= cond2Type = Left "Comparing different data types"
   | HBool `elem` list = Right HBool
   | otherwise = Left (errorExpected HBool)
   where
      cond1Type = typeCheckingCond cond1 [HInt, HChar] hashmap
      cond2Type = typeCheckingCond cond2 [HInt, HChar] hashmap

typeCheckingCond (Le cond1 cond2) list hashmap
   | isLeft cond1Type = cond1Type
   | isLeft cond2Type = cond2Type
   | cond1Type /= cond2Type = Left "Comparing different data types"
   | HBool `elem` list = Right HBool
   | otherwise = Left (errorExpected HBool)
   where
      cond1Type = typeCheckingCond cond1 [HInt, HChar] hashmap
      cond2Type = typeCheckingCond cond2 [HInt, HChar] hashmap

typeCheckingCond (And cond1 cond2) list hashmap
   | isLeft cond1Type = cond1Type
   | isLeft cond2Type = cond2Type
   | cond1Type /= cond2Type = Left "Comparing different data types"
   | HBool `elem` list = Right HBool
   | otherwise = Left (errorExpected HBool)
   where
      cond1Type = typeCheckingCond cond1 [HBool] hashmap
      cond2Type = typeCheckingCond cond2 [HBool] hashmap

typeCheckingCond (Or cond1 cond2) list hashmap
   | isLeft cond1Type = cond1Type
   | isLeft cond2Type = cond2Type
   | cond1Type /= cond2Type = Left "Comparing different data types"
   | HBool `elem` list = Right HBool
   | otherwise = Left (errorExpected HBool)
   where
      cond1Type = typeCheckingCond cond1 [HBool] hashmap
      cond2Type = typeCheckingCond cond2 [HBool] hashmap

typeCheckingCond (Not cond) list hashmap
   | isLeft eitherVarType = eitherVarType
   | HBool `elem` list = Right HBool
   | otherwise = Left (errorExpected HBool)
   where
      eitherVarType = typeCheckingCond cond [HBool] hashmap

typeCheckingCond (Boolean _) list hashmap
   | HBool `elem` list = Right HBool
   | otherwise = Left (errorExpected HBool)

typeCheckingCond (Expr expr) list hashmap = typeCheckingExpr expr list hashmap


-- This function checks whether the expresion given is Nothing or Just expression and then type checks it, in case of an error, then return the error
typeCheckingMayExpr :: Maybe Expr -> [HashmapType] -> Hashmap -> Either String Bool
typeCheckingMayExpr mayExpr list hashmap
   | isJust mayExpr && isLeft eitherVarType = Left (fromLeft "" eitherVarType)
   | isJust mayExpr && elem (fromRight HInt eitherVarType) list = Right True
   | otherwise = Right True
   where
      eitherVarType = typeCheckingExpr (fromJust mayExpr) list hashmap

-- Extract from Array data type the type of array that it is (HInt, HBool, HChar)
fromHArray :: HashmapType -> HashmapType
fromHArray (HArray x) = x
fromHArray x = x

-- This function is important for the array listerals ([2, 3, x+2, y-5]), so that the expressions inside of the array all have the correct data type
-- If something is not right, then return Left error
typeCheckingListExpr :: [Expr] -> [HashmapType] -> Hashmap -> Either String HashmapType
typeCheckingListExpr [x] list hashmap
   | isLeft eitherVarType = eitherVarType
   | otherwise = Right (HArray (fromRight HInt eitherVarType))
   where
      eitherVarType = typeCheckingExpr x [y | (HArray y) <- list] hashmap
typeCheckingListExpr (x:xs) list hashmap
   | isLeft resType = resType
   | isLeft eitherVarType = eitherVarType
   | HArray (fromRight HInt resType) == fromRight HInt eitherVarType = Right (HArray (fromRight HInt resType))
   | otherwise = Left "Array has different data types"
   where
      newList = [y | (HArray y) <- list]
      resType = typeCheckingExpr x newList hashmap
      eitherVarType = typeCheckingListExpr xs list hashmap

-- The function goes through each type of statement there will be inside of the program and does the necessary checks to see if it correctly defined, in case of shadowing then rename the variables
-- Otherwise, it returns the error message of what is wrong with the program
typeCheckingBlock :: [Statement] -> Hashmap -> [VarName] -> Either String [Statement]
typeCheckingBlock [] _ _ = Right []
typeCheckingBlock ((Declaration (Primitive scope varType name mayExpr)):xs) hashmap list
   | isLeft either1 = Left (fromLeft "" either1)
   | checkShadowing hashmap name = case either3 of
                                       Left err -> Left err
                                       Right prog -> Right (Declaration (Primitive scope varType newName mayExpr) : prog)
   | isLeft either2 = either2
   | otherwise = Right (Declaration (Primitive scope varType name mayExpr) : fromRight [] either2)
   where
      either1 = typeCheckingMayExpr mayExpr [changeType varType] hashmap
      either2 = typeCheckingBlock xs (insertHashmap (changeType varType, name) hashmap) list
      either3 = typeCheckingBlock (renameVar name newName xs) (insertHashmap (changeType varType, newName) hashmap) (list ++ [newName])
      newName
         | name `elem` list = nameStart ++ show (read [num] + 1)
         | otherwise = name ++ "_1"
      (nameStart, num) = (init (name :: String), last (name :: String))

typeCheckingBlock ((Declaration (TLock name)):xs) hashmap list
   | checkShadowing hashmap name = typeCheckingBlock (renameVar name newName (Declaration (TLock name):xs)) hashmap (list ++ [newName])
   | isLeft either1 = either1
   | otherwise = Right(Declaration (TLock name) : fromRight [] either1)
   where
      either1 = typeCheckingBlock xs (insertHashmap (HLock, name) hashmap) list
      newName
         | name `elem` list = nameStart ++ show (read [num] + 1)
         | otherwise = name ++ "_1"
      (nameStart, num) = (init (name :: String), last (name :: String))

typeCheckingBlock ((Declaration (Array varType name size mayExpr)):xs) hashmap list
   | checkShadowing hashmap name = typeCheckingBlock (renameVar name newName (Declaration (Array varType name size mayExpr):xs)) hashmap (list ++ [newName])
   | isLeft either1 = Left (fromLeft "" either1)
   | isLeft either2 = Left (fromLeft "" either2)
   | isLeft either3 = either3
   | otherwise = Right (Declaration (Array varType name size mayExpr) : fromRight [] either3)
   where
      either1 = checkArraySize (Array varType name size mayExpr)
      either2 = typeCheckingMayExpr mayExpr [HArray (changeType varType)] hashmap
      either3 = typeCheckingBlock xs (insertHashmap (HArray (changeType varType), name) hashmap) list
      newName
         | name `elem` list = nameStart ++ show (read [num] + 1)
         | otherwise = name ++ "_1"
      (nameStart, num) = (init (name :: String), last (name :: String))

typeCheckingBlock ((Declaration (String name expr)):xs) hashmap list
   | checkShadowing hashmap name = typeCheckingBlock (renameVar name newName (Declaration (String name expr):xs)) hashmap (list ++ [newName])
   | isLeft either1 = Left (fromLeft "" either1)
   | isLeft either2 = either2
   | otherwise = Right (Declaration (String name expr) : fromRight [] either2)
   where
      either1 = checkStringDecl (String name expr)
      either2 = typeCheckingBlock xs (insertHashmap (HString, name) hashmap) list
      newName
         | name `elem` list = nameStart ++ show (read [num] + 1)
         | otherwise = name ++ "_1"
      (nameStart, num) = (init (name :: String), last (name :: String))

typeCheckingBlock ((Assignment (Absolute name expr)):xs) hashmap list
   | isLeft either1 = Left (fromLeft "" either1)
   | isLeft either2 = Left (fromLeft "" either2)
   | isLeft either3 = either3
   | fromRight HInt either1 `elem` [HInt, HBool, HChar] = Right (Assignment (Absolute name expr) : fromRight [] either3)
   | otherwise = Left ("Cannot do absolute assignments to " ++ getStringType (fromRight HInt either1))
   where
      either1 = extractTypeHashmap hashmap name
      either2 = typeCheckingMayExpr (Just expr) [fromRight HInt either1] hashmap
      either3 = typeCheckingBlock xs hashmap list

typeCheckingBlock ((Assignment (Partial name index expr)):xs) hashmap list
   | isLeft either1 = Left (fromLeft "" either1)
   | isLeft either2 = Left (fromLeft "" either2)
   | isLeft either3 = Left (fromLeft "" either3)
   | isLeft either4 = either4
   | fromRight HInt either1 `elem` [HString, HArray HInt, HArray HBool, HArray HChar] = Right (Assignment (Partial name index expr) : fromRight [] either4)
   | otherwise = Left ("Cannot do partial assignments to " ++ getStringType (fromRight HInt either1) ++ name)
   where
      either1 = extractTypeHashmap hashmap name
      either2 = typeCheckingMayExpr (Just index) [HInt] hashmap
      either3 = typeCheckingMayExpr (Just expr) [varType] hashmap
      either4 = typeCheckingBlock xs hashmap list
      HArray varType = fromRight HInt either1

typeCheckingBlock ((If cond block mayBlock):xs) hashmap list
   | isLeft either1 = Left (fromLeft "" either1)
   | isLeft either2 = either2
   | isLeft either4 = either4
   | isJust mayBlock && isLeft either3 = either3
   | isJust mayBlock = Right (If cond (fromRight [] either2) (Just (fromRight [] either3)) : fromRight [] either4)
   | otherwise = Right (If cond (fromRight [] either2) Nothing : fromRight [] either4)
   where
      either1 = typeCheckingCond cond [HBool] hashmap
      either2 = typeCheckingBlock block (newBlockHashmap hashmap) list
      either3 = typeCheckingBlock (fromJust mayBlock) (newBlockHashmap hashmap) list
      either4 = typeCheckingBlock xs hashmap list

typeCheckingBlock ((While cond block):xs) hashmap list
   | isLeft either1 = Left (fromLeft "" either1)
   | isLeft either2 = Left (fromLeft "" either2)
   | isLeft either3 = either3
   | isLeft either4 = either4
   | otherwise = Right (While cond (fromRight [] either3) : fromRight [] either4)
   where
      either1 = typeCheckingCond cond [HBool] hashmap
      either2 = checkWhile block
      either3 = typeCheckingBlock block (newBlockHashmap hashmap) list
      either4 = typeCheckingBlock xs hashmap list

typeCheckingBlock ((Print expr):xs) hashmap list
   | isLeft either1 = Left (fromLeft "" either1)
   | isLeft either2 = either2
   | otherwise = Right (Print expr : fromRight [] either2)
   where
      either1 = typeCheckingMayExpr (Just expr) [HInt, HBool, HChar, HArray HInt, HArray HChar, HArray HBool, HString] hashmap
      either2 = typeCheckingBlock xs hashmap list

typeCheckingBlock ((Thread block):xs) hashmap list
   | isLeft either1 = Left (fromLeft "" either1)
   | isLeft either2 = either2
   | isLeft either3 = either3
   | otherwise = Right (Thread (fromRight [] either2) : fromRight [] either3)
   where
      either1 = checkThread block
      either2 = typeCheckingBlock block (newBlockHashmap hashmap) list
      either3 = typeCheckingBlock xs hashmap list

typeCheckingBlock ((Lock name):xs) hashmap list
   | isLeft either1 = Left (fromLeft "" either1)
   | isLeft either2 = either2
   | fromRight HInt either1 == HLock = Right (Lock name : fromRight [] either2)
   | otherwise = Left ("Cannot lock data type variable " ++ getStringType (fromRight HInt either1))
   where
      either1 = extractTypeHashmap hashmap name
      either2 = typeCheckingBlock xs hashmap list

typeCheckingBlock ((Unlock name):xs) hashmap list
   | isLeft either1 = Left (fromLeft "" either1)
   | isLeft either2 = either2
   | fromRight HInt either1 == HLock = Right (Unlock name : fromRight [] either2)
   | otherwise = Left ("Cannot unlock data type variable " ++ getStringType (fromRight HInt either1))
   where
      either1 = extractTypeHashmap hashmap name
      either2 = typeCheckingBlock xs hashmap list

typeCheckingBlock ((Block block):xs) hashmap list
   | isLeft either1 = either1
   | isLeft either2 = either2
   | otherwise = Right (Block (fromRight [] either1) : fromRight [] either2)
   where
      either1 = typeCheckingBlock block (newBlockHashmap hashmap) list
      either2 = typeCheckingBlock xs hashmap list


-- Checks whever a variable is shadowing another one
checkShadowing :: Hashmap -> VarName -> Bool
checkShadowing (Scope list []) s = False
checkShadowing (Scope list [h]) s = elem s (map snd list) || checkShadowing h s


------------------------------------------------
--                   Renaming                 --
------------------------------------------------


-- Goes through each type of statment and renames where possible with the new name
renameVar :: VarName -> VarName -> [Statement] -> [Statement]
renameVar name newName [] = []
renameVar name newName ((Declaration (Primitive scope varType x mayExpr)):xs) =
   Declaration (Primitive scope varType (mattchingString name newName x) (renameMaybeExpr name newName mayExpr)) : renameVar name newName xs

renameVar name newName ((Declaration (TLock x)):xs) =
   Declaration (TLock (mattchingString name newName x)) : renameVar name newName xs

renameVar name newName ((Declaration (Array varType x size mayExpr)):xs) =
   Declaration (Array varType (mattchingString name newName x) size (renameMaybeExpr name newName mayExpr)) : renameVar name newName xs

renameVar name newName ((Declaration (String x expr)):xs) =
   Declaration (String (mattchingString name newName x) expr) : renameVar name newName xs

renameVar name newName ((Assignment (Absolute x expr)):xs) =
   Assignment (Absolute (mattchingString name newName x) (renameExpr name newName expr)) : renameVar name newName xs
renameVar name newName ((Assignment (Partial x expr1 expr2)):xs) =
   Assignment (Partial (mattchingString name newName x ) (renameExpr name newName expr1) (renameExpr name newName expr2)) : renameVar name newName xs

renameVar name newName ((If cond block mayBlock):xs)
   | isJust mayBlock = If (renameCond name newName cond) (renameVar name newName block) (Just (renameVar name newName (fromJust mayBlock))) : renameVar name newName xs
   | otherwise = If (renameCond name newName cond) (renameVar name newName block) Nothing : renameVar name newName xs

renameVar name newName ((While cond block):xs) = While (renameCond name newName cond) (renameVar name newName block) : renameVar name newName xs
renameVar name newName ((Print x):xs) = Print (renameExpr name newName x) : renameVar name newName xs

renameVar name newName ((Lock x):xs) = Lock (mattchingString name newName x ) : renameVar name newName xs
renameVar name newName ((Unlock x):xs) = Unlock (mattchingString name newName x ) : renameVar name newName xs
renameVar name newName ((Block block):xs) = Block (renameVar name newName block) : renameVar name newName xs
renameVar name newName ((Thread block):xs) = Thread (renameVar name newName block) : renameVar name newName xs


-- Checks whether it is Nothing, then returns Nothing, otherwise tries to rename what is inside of expr
renameMaybeExpr :: VarName -> VarName -> Maybe Expr -> Maybe Expr
renameMaybeExpr name newName Nothing = Nothing
renameMaybeExpr name newName (Just expr) = Just (renameExpr name newName expr)

-- Function compares name with a given string, and in case true then return the new name, otherwise return the default name
mattchingString :: VarName -> VarName -> VarName -> VarName
mattchingString name newName x
   | x == name = newName
   | otherwise = x

-- Goes through each type of expression and renames where it is possible with the new name
renameExpr :: VarName -> VarName -> Expr -> Expr
renameExpr name newName (Var x) = Var (mattchingString name newName x)
renameExpr name newName (Add expr1 expr2) = Add (renameExpr name newName expr1) (renameExpr name newName expr2)
renameExpr name newName (Mult expr1 expr2) = Mult (renameExpr name newName expr1) (renameExpr name newName expr2)
renameExpr name newName (Sub expr1 expr2) = Sub (renameExpr name newName expr1) (renameExpr name newName expr2)
renameExpr name newName (Div expr1 expr2) = Div (renameExpr name newName expr1) (renameExpr name newName expr2)
renameExpr name newName (Condition cond) = Condition (renameCond name newName cond)
renameExpr name newName (ArrayLiteral list) = ArrayLiteral (renameExprList name newName list)
renameExpr name newName (ArrayIndex x expr) = ArrayIndex (mattchingString name newName x) (renameExpr name newName expr)
renameExpr name newName x = x

-- Goes through each expression in array and then applies renameExpr
renameExprList :: VarName -> VarName -> [Expr] -> [Expr]
renameExprList name newName = map (renameExpr name newName)

-- Goes through each type of expression and renames where it is possible with the new name
renameCond :: VarName -> VarName -> Condition -> Condition
renameCond name newName (Eq cond1 cond2) = Eq (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (Neq cond1 cond2) = Neq (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (Gt cond1 cond2) = Gt (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (Lt cond1 cond2) = Lt (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (Ge cond1 cond2) = Ge (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (Le cond1 cond2) = Le (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (And cond1 cond2) = And (renameCond name newName cond1) (renameCond name newName cond2)
renameCond name newName (Or cond1 cond2) = Or (renameCond name newName cond1) (renameCond name newName cond2)

renameCond name newName (Not cond) = Not (renameCond name newName cond)
renameCond name newName (Expr expr) = Expr (renameExpr name newName expr)
renameCond name newName x = x