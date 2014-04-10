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
       , opStart = oneOf "=+-*/" -- toto nevim, co znamena
       , opLetter = opStart aelDef -- toto nevim, co znamena
       , reservedOpNames = ["=", "+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!="]
       -- TODO: "return" v zadani neni, ale IMHO by mel byt 
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
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

aep = do
    whiteSpace
    ast <- cmd
    eof
    return ast
    <?> "aep"

data Type = Integer | Double | String

data Command = DefineVar String Type -- je potreba taky empty?
     -- | DefineFun String [ (String, Type) ] Command -- TODO: toto je divny, pujde to tak?
     | Assign String Expr
     | Print Expr
     | Scan String
     | Seq [ Command ]
     | If BoolExpr Command Command -- mozna ma byt "Seq [ Command ]"? so many questions!
     | While BoolExpr Command  -- ditto
     | Return Expr
     -- tyhle veci si uplne nedokazu predstavit, jak maji fungovat
     -- maji byt funkce v AST? a jak?
     -- mozna by bylo dobre mit AST, kde bude deklarace/definice funkce
     -- a pak mit dve tabulky - tabulku promennych a tabulku funkci
     -- kdyz program narazi na deklaraci, zapise do tabulky funkci hlavicku,
     -- kdyz narazi na definici, zapise si do ni cele telo
     -- no a pri vykonavani se pujde po AST a dal vubec nevim jak to bude fungovat
     --
     -- mozna by nemusel existovat zadny "globalni" syntakticky strom
     -- a misto toho pole syntaktickych stromu, kde kazdy reprezentuje funkci
     -- jednoduse by se pak resil main - startovaci misto programu
     -- nicmene pak by zas bylo pain in the ass resit, jestli na danem miste
     -- volana funkce uz byla nebo nebyla deklarovana
     --
     -- deklarace funkci by mozna vubec nemela byt prikaz, kdyz ani
     -- nemame first-class functions
     | Declare String [ ( String, Type ) ] -- TODO: toto je asi uplne blbe napsany, ale snad z toho bude jasny, co jsme mel na mysli a pak to pujde prepsat spravne
     | Call String [ String ]

data Expr = Const Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving Show

type SymbolTable = ([(String,Int)],[[(String,Int)]])

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
    reserved "int"
    i <- identifier
    semi
    return $ DefineVar i Integer
    <|> do
    reserved "double"
    i <- identifier
    semi
    return $ DefineVar i Double
    <|> do
    reserved "string"
    i <- identifier
    semi
    return $ DefineVar i String
    <|> do
    i <- identifier
    reserved "="
    e <- expr
    semi
    return $ Assign i e
    <|> do
    reserved "if"
    b <- parens $ boolExpr
    seq1 <- braces $ cmd -- TODO: toto mozna udelat zvlast jako parsovani seq? ale jak?
    reserved "else"
    seq2 <- braces $ cmd
    return $ If b seq1 seq2
    <|> do
    reserved "while"
    b <- parens $ boolExpr
    seq <- braces $ cmd
    return $ While b seq
    <|> do
    reserved "return"
    e <- expr
    semi
    return $ Return e
    <?> "command"

-- type SymbolTable = [(String, Type, )] -- wtf, to musim mit datovy typ, spojujici String, Double a Integer?

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
          interpret ([],[[]]) ast -- prvni je tabulka symbolu, druhe tabulka funkci?
