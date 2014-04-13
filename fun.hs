module Main ( main ) where

import System.Environment ( getArgs )
import System.IO

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

aelDef = emptyDef
       { commentStart = "/*"
       , commentEnd = "*/"
       , commentLine = "//"
       , nestedComments = False
       , identStart = letter <|> char '_'
       , identLetter = alphaNum <|> char '_'
       , opStart = oneOf "=+-*/"
       , opLetter = opStart aelDef
       , reservedOpNames = ["=", "+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!="]
       , reservedNames = ["double", "else", "if", "int", "print", "scan", "string", "while", "return"]
       , caseSensitive = True
       }

lexer = P.makeTokenParser aelDef

-- mozna by se hodily dalsi: string, operator, float...
-- viz http://hackage.haskell.org/package/parsec-3.0.0/docs/Text-Parsec-Token.html
whiteSpace = P.whiteSpace lexer
integer = P.integer lexer
parens = P.parens lexer
braces = P.braces lexer
semi = P.semi lexer
comma = P.comma lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

data MultiValue = UndefInt | UndefStr | UndefDouble | IntegerValue Integer | DoubleValue Double | StringValue String deriving (Show, Eq)

data Command = DefineVar String MultiValue -- je potreba taky empty?
     | Assign String Expr
     | Print Expr
     | Scan String
     | Seq [ Command ] -- hele je seq vubec potreba? odpovim si sam, asi je
     -- -| If BoolExpr (Seq [ Command ]) (Seq [ Command ]) -- WAT? ono to nejde..
     -- tak jinak
     | If Expr Command Command
     -- | If BoolExpr [ Command ] [ Command ]  -- WAT? ono to nejde..
     -- | While Expr [ Command ]  -- ditto
     | While Expr Command  -- ditto
     | Return Expr
     | Declare String [ ( String, MultiValue ) ] -- TODO: toto je asi uplne blbe napsany, ale snad z toho bude jasny, co jsme mel na mysli a pak to pujde prepsat spravne
     -- holy fucking shit
     --          nazev  parametry    telo     to je hnuj
     | Function String [ Command ] Command
     | MainF Command
     | Call String [ String ]
     deriving Show

data Expr = Const Integer -- TODO: tady ma byt MultiValue
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Equal Expr Expr
  | NotEqual Expr Expr
  | Greater Expr Expr
  | Lesser Expr Expr
  | GreaterOrEqual Expr Expr
  | LesserOrEqual Expr Expr
  deriving Show


-- ------------------------------------------------------------------------- --
-- ------------------------- SYMBOL TABLE OPERATIONS ----------------------- --
-- ------------------------------------------------------------------------- --

type VariableTable = [(String,MultiValue)]
type GlobalTable = VariableTable
type LocalTable = [VariableTable]
type SymbolTable = (GlobalTable,LocalTable)

expr = buildExpressionParser operators term where
  operators = [
      [ op "*" Mult, op "/" Div ],
      [ op "+" Add, op "-" Sub ],
      [ op "<" Lesser, op "<=" LesserOrEqual, op ">" Greater, op ">=" GreaterOrEqual,
        op "==" Equal, op "!=" NotEqual]
    ]
  op name fun =
    Infix ( do { reservedOp name; return fun } ) AssocLeft

term = do
    i <- integer
    return $ Const $ fromInteger i
  <|> do
    v <- identifier
    return $ Var v
  <|> parens expr
  <?> "term"

varDeclarationType = do
    reserved "int"
    i <- identifier
    return $ DefineVar i UndefInt
    <|> do
    reserved "double"
    i <- identifier
    return $ DefineVar i UndefDouble
    <|> do
    reserved "string"
    i <- identifier
    return $ DefineVar i UndefStr
    <?> "variable declaration, no semi"

varDeclarationLine = do
  var <- varDeclarationType
  semi
  return var
  <?> "variable declaration, one per line"           

cmd = do
    reserved "print"
    e <- parens $ expr
    semi
    return $ Print e
    <|> do
    reserved "scan"
    i <- parens $ identifier
    semi
    return $ Scan i
    <|> do
    i <- identifier
    reserved "="
    e <- expr
    semi
    return $ Assign i e
    <|> do
    reserved "if"
    b <- parens $ expr
    seq1 <- braces $ many cmd -- TODO: toto mozna udelat zvlast jako parsovani seq? ale jak?
    reserved "else"
    seq2 <- braces $ many cmd
    return $ If b (Seq seq1) (Seq seq2)
    <|> do
    reserved "while"
    b <- parens $ expr
    seq <- braces $ many cmd
    return $ While b (Seq seq)
    <|> do
    reserved "return"
    e <- expr
    semi
    return $ Return e
    <?> "command"


-- toto jeste zmenit
funcBody = do
    vars <- many $ varDeclarationLine
    seq <- many cmd
    return $ Seq (vars++seq) -- TODO: toto mozna bude stacit takto?


mainAST = do
    reserved "int"
    -- hm...
    string "main" 
    -- to zakomentovany haze citelnejsi chybu, ale bez radku a sloupce
    -- i <- identifier
    -- if i /= "main"
    -- then error "Expecting main function"
    -- else do
    _ <- parens $ sepBy varDeclarationType comma -- zatim jen placeholder, nevim jak udelat prazdny zavorky
    seq <- braces $ funcBody
    return $ MainF seq
    <?> "main"


-- zatim jen int funkce, poresit nadtyp typu (podobne jak je v pasi.hs to PTypes)
funcDeclaration = do
  reserved "int"
  i <- identifier
  vars <- parens $ sepBy varDeclarationType comma
  semi  -- nebo _ <- semi ?? kdo vi
  return $ Function i vars (Seq [])
  <?> "function definition"

funcDefinition = do
  reserved "int"
  i <- identifier
  vars <- parens $ sepBy varDeclarationType comma
  seq <- braces $ funcBody
  if i /= "main"
  then return $ Function i vars seq
  else return $ MainF seq
  <?> "function declaration"  


funcAST = do
  -- jestli chapu dobre, try je dobrej k look ahead
  try funcDeclaration
  <|>
  funcDefinition
  <?> "function"

aep = do
    whiteSpace
    _ <- many $ try varDeclarationLine
    asts <- many funcAST
    -- main <- mainAST 
    eof
    return asts
    <?> "aep"


getMain :: [Command] -> Command
getMain [] = error "Yo mama is so dumb, she forgot main!"
getMain ((Function _ _ _) : asts) = getMain asts
getMain (m@(MainF _) : _) = m


parseAep input file =
         case parse aep file input of
              Left e -> error $ show e
              Right ast -> ast

getSt :: SymbolTable -> String -> MultiValue
getSt ([], ([]:_)) variable = error $ "Variable \"" ++ variable ++ "\" not in scope"
getSt ([], (((name, value):xs):rest)) variable =
  if variable == name then value
  else getSt ([], (xs:rest)) variable
getSt (((name, value):xs), rest) variable =
  if variable == name then value
  else getSt (xs, rest) variable

setVariableInList :: VariableTable -> String -> MultiValue -> Maybe VariableTable
setVariableInList [] variable _ = Nothing
setVariableInList (first@(name, _):xs) variable value =
                  if name == variable then Just ((name, value):xs)
                  else case setVariableInList xs variable value of
                            Nothing -> Nothing
                            Just a -> Just (first:a)

-- TODO: kontrolovat datovy typy
-- "tvrda" varianta, ktera promennou nevytvari, jen nastavuje
setSt :: SymbolTable -> String -> MultiValue -> SymbolTable
setSt (global, local@(head:rest)) variable value =
      case setVariableInList head variable value of
           Nothing -> case setVariableInList global variable value of
                           Nothing -> error $ "Variable \"" ++ variable ++ "\" not in scope"
                           Just result -> (result, local)
           Just result -> (global, (result:rest))

-- TODO: mozna nejaka kontrola neexistence?
insertStLocal :: SymbolTable -> String -> MultiValue -> SymbolTable
insertStLocal (global, (head:rest)) name value = (global, ((name, value):head):rest)

-- TODO: taky prebirat funkci a rovnou vytvorit a naplnit argumenty
prepareStForCall :: SymbolTable -> [Command] -> IO SymbolTable
prepareStForCall (global, local) defs = do
                 interpret (global, ([]:local)) (Seq defs) []

add :: MultiValue -> MultiValue -> MultiValue
add (IntegerValue a) (IntegerValue b) = IntegerValue (a + b)
add (DoubleValue a) (DoubleValue b) = DoubleValue (a + b)
add (StringValue a) (StringValue b) = StringValue (a ++ b)

sub :: MultiValue -> MultiValue -> MultiValue
sub (IntegerValue a) (IntegerValue b) = IntegerValue (a - b)
sub (DoubleValue a) (DoubleValue b) = DoubleValue (a - b)
-- TODO: i pro integer s doublem?

mult :: MultiValue -> MultiValue -> MultiValue
mult (IntegerValue a) (IntegerValue b) = IntegerValue (a * b)
mult (DoubleValue a) (DoubleValue b) = DoubleValue (a * b)
-- TODO: i pro integer s doublem?

divide :: MultiValue -> MultiValue -> MultiValue
divide (IntegerValue a) (IntegerValue b) = IntegerValue (quot a  b) -- TODO: nemusi se tady prevadet z double na integer?
divide (DoubleValue a) (DoubleValue b) = DoubleValue (a / b)

comp :: Ordering -> MultiValue -> MultiValue -> MultiValue
comp order (IntegerValue a) (IntegerValue b) =
     if (compare a b) == order then IntegerValue 1
     else IntegerValue 0
comp order (DoubleValue a) (DoubleValue b) =
     if (compare a b) == order then IntegerValue 1
     else IntegerValue 0
comp order (StringValue a) (StringValue b) =
     if (compare a b) == order then IntegerValue 1
     else IntegerValue 0
comp _ _ _ = error "Cannot compare"

orMultiVal :: MultiValue -> MultiValue -> MultiValue
orMultiVal (IntegerValue a) (IntegerValue b) =
           if (a /= 0) || (b /= 0) then (IntegerValue 1)
           else (IntegerValue 0)

notMultiVal :: MultiValue -> MultiValue
notMultiVal (IntegerValue 0) = IntegerValue 1
notMultiVal _ = IntegerValue 0

lesser = comp LT
greater = comp GT
equal = comp EQ

eval :: SymbolTable -> Expr -> MultiValue
eval st (Const i) = IntegerValue i -- TODO: toto je zatim blbe, nemusi to byt jenom Integer, ale aby to slo zkompilovat...
eval st (Var v) = getSt st v
eval st (Add e1 e2) = (eval st e1) `add` (eval st e2)
eval st (Sub e1 e2) = (eval st e1) `sub` (eval st e2)
eval st (Mult e1 e2) = (eval st e1) `mult` (eval st e2)
eval st (Div e1 e2) = (eval st e1) `divide` (eval st e2) -- a co deleni nulou?
eval st (Lesser e1 e2) = (eval st e1) `lesser` (eval st e2)
eval st (Greater e1 e2) = (eval st e1) `greater` (eval st e2)
eval st (Equal e1 e2) = (eval st e1) `equal` (eval st e2)
eval st (NotEqual e1 e2) = notMultiVal $ (eval st e1) `equal` (eval st e2)
eval st (LesserOrEqual e1 e2) =
     ((eval st e1) `lesser` (eval st e2)) `orMultiVal` ((eval st e1) `equal` (eval st e2))
eval st (GreaterOrEqual e1 e2) =
     ((eval st e1) `greater` (eval st e2)) `orMultiVal` ((eval st e1) `equal` (eval st e2))

evaluateBool :: SymbolTable -> Expr -> Bool
evaluateBool st expr = (eval st expr) /= IntegerValue 0

--    tabulka symbolu - aktualni prikaz - tabulka funkci - vystup
interpret :: SymbolTable -> Command -> [Command] -> IO SymbolTable
interpret st (DefineVar name value) functions = do
          putStrLn "define var"
          return $ insertStLocal st name value
interpret st (Assign name e) functions = do
          putStrLn "assing var"
          return $ setSt st name $ eval st e
interpret st (Print e) functions = do
          putStrLn "print var"
          putStrLn $ show $ eval st e
          return st
interpret st (Scan name) functions = do
          putStrLn "scan var"
          return st
interpret st (Seq []) functions = do
          putStrLn "last seq"
          return st
interpret st (Seq (first:others)) functions = do
          putStrLn "seq"
          newst <- interpret st first functions
          interpret newst (Seq others) functions
interpret st (If e seq1 seq2) functions
          | evaluateBool st e = do
                         putStrLn "if first"
                         interpret st seq1 functions
          | otherwise = do
                      putStrLn "if second"
                      interpret st seq2 functions
interpret st loop@(While e seq) funcions
          | evaluateBool st e = do
                         putStrLn "while loop"
                         newst <- interpret st seq funcions
                         interpret newst loop funcions
          | otherwise = do
                      putStrLn "while end"
                      return st
interpret st (MainF seq) functions = do
          putStrLn "main"
          newst <- prepareStForCall st []
          interpret newst seq functions
interpret st (Function _ params seq) functions = do
          putStrLn "func call"
          -- newst <- prepareStForCall st params -- TODO: toto ma byt v call, kvuli nastavenejm promenejm
          interpret newst seq functions
interpret st _ _ = do
          putStrLn "other"
          return st

main = do
     args <- getArgs
     if length args /= 1
     then error "Input file should be specified"
     else do
          let fileName = args!!0
          input <- readFile fileName
          let asts = parseAep input fileName
          putStrLn $ show asts
          interpret ([],[[]]) (getMain asts) asts -- prvni je tabulka symbolu, druhe tabulka funkci?
