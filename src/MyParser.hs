module MyParser where
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Combinator (many1)
import Control.Arrow (left)

import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Maybe
import Data.List



------------------------------------------------
--                    EDSL                    --
------------------------------------------------

newtype Program = Program Block deriving (Show, Eq)

-- data Scope, between the {}
type Block = [Statement]
type VarName = String
type ArrSize = Integer

-- statement is a line of code
data Statement = Declaration Declaration
               | Assignment  Assignment
               | If          Condition Block (Maybe Block)
               | While       Condition Block
               | Print       Expr
               | Thread      Block
               | Lock        VarName
               | Unlock      VarName
               | Block       Block
               deriving (Show, Eq)


data Scope  = Global | Local deriving (Show, Eq)
data MyType = TInt | TBool | TChar deriving (Show, Eq)

-- declaration of variables
data Declaration = Primitive Scope MyType VarName (Maybe Expr)
                 | TLock     VarName               -- always global
                 | Array     MyType VarName ArrSize (Maybe Expr)
                 | String    VarName Expr          -- Expr must be StringLiteral; String is immutable
                 deriving (Show, Eq)

-- assignment of variables
data Assignment = Absolute VarName Expr            -- includes cases where x = y, x = y - 3 ...
                | Partial  VarName Expr Expr       -- important for array value changing at index:  a[1] = 24
                deriving (Show, Eq)

-- Expr is rhs of assignment
data Expr = Const Integer
          | Char Char
          | Var  String
          | Add  Expr Expr
          | Mult Expr Expr
          | Sub  Expr Expr
          | Div  Expr Expr
          | Condition Condition
          -- bonus (not supported)
          | ArrayLiteral [Expr]              -- create an array with elements [3, 5, 90+13, 24, 15]
          | ArrayIndex VarName Expr          -- get values of array: y = x[1]
          | StringLiteral String             -- a string "Some random text" 
          deriving (Show, Eq)


-- 1 == 2 == False works like (1 == 2) == False -> False == False -> True
data Condition = Eq Condition Condition
               | Neq Condition Condition
               | Gt Condition Condition
               | Lt Condition Condition
               | Ge Condition Condition
               | Le Condition Condition
               | And Condition Condition
               | Or Condition Condition
               | Not Condition
               | Boolean Bool
               | Expr Expr
               deriving (Show, Eq)


---------------------------------------------
--                   LEXER                 --
---------------------------------------------

languageDef =
  emptyDef { Token.commentLine = "//"
           , Token.identStart = letter
           , Token.identLetter = alphaNum
           , Token.reservedNames =
              [ "while", "if", "else", "int", "char", "bool", "String", "Lock", "lock", "unlock", "thread", "global", "true", "false"]
           , Token.reservedOpNames =
              [ "-", "+", "*", "/", "&&", "||", "==", "!=", "<", ">", "<=", ">=", "!"]
           , Token.caseSensitive = True
  }

lexer = Token.makeTokenParser languageDef

-- Create functions for all types of tokens
identifier    = Token.identifier lexer
integer       = Token.integer lexer
parens        = Token.parens lexer
braces        = Token.braces lexer
brackets      = Token.brackets lexer
symbol        = Token.symbol lexer
semi          = Token.semi lexer
dot           = Token.dot lexer
commaSep      = Token.commaSep lexer
charLiteral   = Token.charLiteral lexer
stringLiteral = Token.stringLiteral lexer
reserved      = Token.reserved lexer
whiteSpace    = Token.whiteSpace lexer


------------------------------------------------
--                   PARSER                   --
------------------------------------------------

-- Parses an entire program, ensuring it consumes all input.
program :: Parser Program
program =  whiteSpace *> (Program <$> block) <* eof

-- we did not define block as = braces (many statement) because we need to use block for our global scope
-- Parses a block of code, which consists of multiple statements.
block :: Parser Block
block = many statement

-- Parses a single statement, attempting different types of statements in order.
statement :: Parser Statement
statement =  try (Declaration <$> (declaration <* semi))
         <|> try (Assignment <$> (assignment <* semi))
         <|> try (If <$> (reserved "if" *> parens condition) <*> braces block <*> optionMaybe (reserved "else" *> braces block))
         <|> try (While <$> (reserved "while" *> parens condition) <*> braces block)
         <|> try (Print <$> (reserved "print" *> parens expr <* semi))
         <|> try (Thread <$> (reserved "thread" *> braces block))
         <|> try (Lock <$> (identifier <* (dot *> reserved "lock" *> semi)))
         <|> try (Unlock <$> (identifier <* (dot *> reserved "unlock" *> semi)))
         <|> Block <$> braces block

-- Parses a declaration statement, which could be an array, lock, primitive type, or string.
declaration :: Parser Declaration
declaration = try (Array <$> parserType <*> identifier <*> brackets integer <*> optionMaybe (symbol "=" *> expr))
          <|> try (TLock <$> (reserved "Lock" *> identifier))
          <|> try (Primitive <$> scope <*> parserType <*> identifier <*> optionMaybe (symbol "=" *> expr))
          <|> String <$> (reserved "String" *> identifier) <*> (reserved "=" *> expr)

-- Parses the scope of a variable, either global or local.
scope :: Parser Scope
scope =  try (Global <$ reserved "global")
     <|> pure Local

-- Parses the type of a variable, which could be int, bool, or char.
parserType :: Parser MyType
parserType = try (TInt <$ reserved "int")
         <|> try (TBool <$ reserved "bool")
         <|> TChar <$ reserved "char"

-- Parses an assignment statement, which could be partial (array index) or absolute (variable).
assignment :: Parser Assignment
assignment =  try (Partial <$> identifier <*> brackets expr <*> (symbol "=" *> expr))
          <|> Absolute <$> identifier <*> (symbol "=" *> expr)

-- Parses an expression, allowing for addition and subtraction.
expr :: Parser Expr
expr = chainl1 term (addOp <|> subOp)

-- Parses a term in an expression, allowing for multiplication and division.
term :: Parser Expr
term = chainl1 factor (multOp <|> divOp)

-- Parses a factor in an expression, which could be various types of literals, variables, or nested expressions.
factor :: Parser Expr
factor = try arrayLiteral
     <|> try arrayIndex
     <|> try (Condition <$> boolean)
     <|> try parseConst
     <|> try var
     <|> try parseChar
     <|> try parseString
     <|> try (parens expr)
     <|> parens (Condition <$> condition)

-- Parses a multiplication operator.
multOp :: Parser (Expr -> Expr -> Expr)
multOp = Mult <$ symbol "*"

-- Parses a division operator.
divOp :: Parser (Expr -> Expr -> Expr)
divOp = Div <$ symbol "/"

-- Parses an integer constant.
parseConst :: Parser Expr
parseConst = Const <$> integer

-- Parses a variable
var :: Parser Expr
var = Var <$> identifier

-- Parses an addition operator.
addOp :: Parser (Expr -> Expr -> Expr)
addOp = Add <$ symbol "+"

-- Parses a subtraction operator.
subOp :: Parser (Expr -> Expr -> Expr)
subOp = Sub <$ symbol "-"

-- Parses a character literal.
parseChar :: Parser Expr
parseChar = Char <$> charLiteral

-- Parses a string literal.
-- stringLiteral -> from ParSec
parseString :: Parser Expr
parseString = StringLiteral <$> stringLiteral

-- Parses an array literal.
arrayLiteral :: Parser Expr
arrayLiteral = ArrayLiteral <$> brackets (commaSep expr)

-- Parses an array index access.
arrayIndex :: Parser Expr
arrayIndex = ArrayIndex <$> identifier <*> brackets expr

-- Parses a condition expression, allowing for comparisons and boolean logic.
conditionExpr :: Parser Condition
conditionExpr =  try (chainl1 (Expr <$> expr)
                 (try eq <|> try neq <|> try ge <|> try le <|> try gt <|> try lt))
             <|> try boolean
             <|> parens condition

-- Parses a condition, supporting negation, conjunction, and disjunction.
condition :: Parser Condition
condition = try parseNot
        <|> chainl1 conditionExpr (parseAnd <|> parseOr)


-- Parses an equality comparison.
eq :: Parser (Condition -> Condition -> Condition)
eq = Eq <$ symbol "=="

-- Parses an inequality comparison.
neq :: Parser (Condition -> Condition -> Condition)
neq = Neq <$ symbol "!="

-- Parses a greater-than comparison.
gt :: Parser (Condition -> Condition -> Condition)
gt = Gt <$ symbol ">"

-- Parses a less-than comparison.
lt :: Parser (Condition -> Condition -> Condition)
lt = Lt <$ symbol "<"

-- Parses a greater-than-or-equal-to comparison.
ge :: Parser (Condition -> Condition -> Condition)
ge = Ge <$ symbol ">="

-- Parses a less-than-or-equal-to comparison.
le :: Parser (Condition -> Condition -> Condition)
le = Le <$ symbol "<="

-- Parses a logical AND operator.
parseAnd :: Parser (Condition -> Condition -> Condition)
parseAnd = And <$ symbol "&&"

-- Parses a logical OR operator.
parseOr :: Parser (Condition -> Condition -> Condition)
parseOr = Or <$ symbol "||"

-- Parses a logical NOT operator.
parseNot :: Parser Condition
parseNot = Not <$> (symbol "!" *> parens condition)

-- Parses a boolean literal (true or false).
boolean :: Parser Condition
boolean =  try (Boolean <$> (reserved "true" *> pure True))
       <|> Boolean <$> (reserved "false" *> pure False)


