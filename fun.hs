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

data Command = DefineVar String Type -- je potreba taky empty?
     -- | DefineFun String [ (String, Type) ] Command -- TODO: toto je divny, pujde to tak?
     | Assign String Expr
     | Print Expr
     | Scan String
     | Seq [ Command ]
     -- -| If BoolExpr (Seq [ Command ]) (Seq [ Command ]) -- WAT? ono to nejde..
     | If BoolExpr [ Command ] [ Command ]
     | While BoolExpr [ Command ]  -- ditto
     | Return Expr
     | Declare String [ ( String, Type ) ] -- TODO: toto je asi uplne blbe napsany, ale snad z toho bude jasny, co jsme mel na mysli a pak to pujde prepsat spravne
     | Call String [ String ]
     | Main Command
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

type SymbolTable = ([(String,Type)],[[(String,Type)]])



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
    return $ If b seq1 seq2
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
    _ <- many varDeclarationLine
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
    return $ Main seq
    <?> "main"


aep = do
    whiteSpace
    main <- mainAST 
    eof
    return main
    <?> "aep"



parseAep input file =
         case parse aep file input of
              Left e -> error $ show e
              Right ast -> ast

interpret :: SymbolTable -> Command -> IO SymbolTable
interpret ts _ = return ts

main = do
     args <- getArgs
     if length args /= 1
     then error "Input file should be specified"
     else do
          let fileName = args!!0
          input <- readFile fileName
          let ast = parseAep input fileName
          putStrLn $ show ast
          interpret ([],[[]]) ast -- prvni je tabulka symbolu, druhe tabulka funkci?
