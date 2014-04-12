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

data Type = Integer | Double | String deriving Show
data MultiValue = Undefined | IntegerValue Integer | DoubleValue Double | StringValue String deriving Show

data Command = DefineVar String Type -- je potreba taky empty?
    -- holy fucking shit  
    --          nazev  parametry    telo     to je hnuj
     | Function String [ Command ] Command -- TODO: toto je divny, pujde to tak?
     | Assign String Expr
     | Print Expr
     | Scan String
     | Seq [ Command ] -- hele je seq vubec potreba? odpovim si sam, asi je
     -- -| If BoolExpr (Seq [ Command ]) (Seq [ Command ]) -- WAT? ono to nejde..
     -- tak jinak
     | If BoolExpr Command Command 
     -- | If BoolExpr [ Command ] [ Command ]  -- WAT? ono to nejde..
     | While BoolExpr [ Command ]  -- ditto
     | Return Expr
     | Declare String [ ( String, Type ) ] -- TODO: toto je asi uplne blbe napsany, ale snad z toho bude jasny, co jsme mel na mysli a pak to pujde prepsat spravne
     | Call String [ String ]
     | MainF Command
     deriving Show

data Expr = Const Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
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
      [ op "+" Add, op "-" Sub ]
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

data BoolExpr = Equal Expr Expr
              | NotEqual Expr Expr
              | Greater Expr Expr
              | Lesser Expr Expr
              | GreaterOrEqual Expr Expr
              | LesserOrEqual Expr Expr
	deriving Show

boolExpr = do
    e1 <- expr
    o <- relOp
    e2 <- expr
    return $ o e1 e2
  <?> "boolean expression"
  where
    relOp = ro' "==" Equal
      <|> ro' "!=" NotEqual
      <|> ro' ">" Greater
      <|> ro' "<" Lesser
      <|> ro' ">=" GreaterOrEqual
      <|> ro' "<=" LesserOrEqual
      <?> "relational operator"
    ro' name fun = do
      reservedOp name
      return fun

varDeclarationType = do
    reserved "int"
    i <- identifier
    return $ DefineVar i Integer
    <|> do
    reserved "double"
    i <- identifier
    return $ DefineVar i Double
    <|> do
    reserved "string"
    i <- identifier
    return $ DefineVar i String
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
    b <- parens $ boolExpr
    seq1 <- braces $ many cmd -- TODO: toto mozna udelat zvlast jako parsovani seq? ale jak?
    reserved "else"
    seq2 <- braces $ many cmd
    return $ If b (Seq seq1) (Seq seq2)
    <|> do
    reserved "while"
    b <- parens $ boolExpr
    seq <- braces $ many cmd
    return $ While b seq
    <|> do
    reserved "return"
    e <- expr
    semi
    return $ Return e
    <?> "command"


-- toto jeste zmenit
funcBody = do
    _ <- many $ varDeclarationLine
    seq <- many cmd
    return $ Seq seq



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

-- "tvrda" varianta, ktera promennou nevytvari, jen nastavuje
setSt :: SymbolTable -> String -> MultiValue -> SymbolTable
setSt (global, local@(head:rest)) variable value =
      case setVariableInList head variable value of
           Nothing -> case setVariableInList global variable value of
                           Nothing -> error $ "Variable \"" ++ variable ++ "\" not in scope"
                           Just result -> (result, local)
           Just result -> (global, (result:rest))

interpret :: SymbolTable -> Command -> IO SymbolTable
interpret ts _ = return ts
--interpret ts (Print e) = putStrLn $ show $ eval e

main = do
     args <- getArgs
     if length args /= 1
     then error "Input file should be specified"
     else do
          let fileName = args!!0
          input <- readFile fileName
          let asts = parseAep input fileName
          putStrLn $ show asts
          interpret ([],[[]]) (getMain asts) -- prvni je tabulka symbolu, druhe tabulka funkci?
