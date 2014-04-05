-- (c) 2011 Stanislav Zidek

module Main( main ) where

import System.Environment( getArgs )
import System.IO

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

aelDef = emptyDef
	{ commentStart   = "/*"
	, commentEnd     = "*/"
	, commentLine    = "//"
	, nestedComments = False
	, identStart     = letter <|> char '_'
	, identLetter    = alphaNum <|> char '_'
	, opStart        = oneOf "=+*-"
	, opLetter       = opStart aelDef
	, reservedOpNames= [ "=", "+", "*", "-", "/", "==", "!=" ]
	, reservedNames  = [ "print", "scan", "while", "do", "if", "then", "else" ]
	, caseSensitive  = True
	}

lexer = P.makeTokenParser aelDef

whiteSpace= P.whiteSpace lexer
integer   = P.integer lexer
parens    = P.parens lexer
braces    = P.braces lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer

aep = do
  whiteSpace
  ast <- cmd
  eof
  return ast
  <?> "aep"

data Command = Empty
  | Assign String Expr
  | Print Expr
  | Scan String
  | Seq [ Command ]
  | If BoolExpr Command Command
  | While BoolExpr Command 
	deriving Show

cmd = do
    semi
    return Empty
  <|> do
    reserved "print"
    e <- expr
    semi
    return $ Print e
  <|> do
    reserved "scan"
    i <- identifier
    semi
    return $ Scan i
  <|> do
    i <- identifier
    reservedOp "="
    e <- expr
    semi
    return $ Assign i e
  <|> do
    reserved "if"
    b <- boolExpr
    reserved "then"
    c1 <- cmd
    reserved "else"
    c2 <- cmd
    return $ If b c1 c2
  <|> do
    reserved "while"
    b <- boolExpr
    reserved "do"
    c <- cmd
    return $ While b c
  <|> do
    seq <- braces $ many cmd
    return $ Seq seq
  <?> "command"

data Expr = Const Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving Show

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

type SymbolTable = [(String, Int)]

set :: SymbolTable -> String -> Int -> SymbolTable
set [] var val = [(var, val)]
set (s@(v,_):ss) var val =
	if v == var
		then (var, val):ss
		else s : set ss var val

get :: SymbolTable -> String -> Int		
get [] _ = error "Not found"
get (s@(var, val):ss) v =
	if v == var
		then val
		else get ss v

evaluate :: SymbolTable -> Expr -> Int
evaluate ts (Const i) = i
evaluate ts (Var v) = get ts v
evaluate ts (Add e1 e2) = (evaluate ts e1) + (evaluate ts e2)
evaluate ts (Sub e1 e2) = (evaluate ts e1) - (evaluate ts e2)
evaluate ts (Mult e1 e2) = (evaluate ts e1) * (evaluate ts e2)

decide :: SymbolTable -> BoolExpr -> Bool
decide ts (Equal a b) = evaluate ts a == evaluate ts b
decide ts (NotEqual a b) = evaluate ts a /= evaluate ts b

interpret :: SymbolTable -> Command -> IO SymbolTable

interpret ts (Empty) = return ts

interpret ts (Assign v e) = return $ set ts v $ evaluate ts e

interpret ts (Print e) = do
  putStrLn $ show $ evaluate ts e
  return ts

interpret ts (Scan v) = do
  i <- readLn :: IO Int
  return $ set ts v i

interpret ts (If cond c1 c2) = do
  if decide ts cond
    then interpret ts c1
    else interpret ts c2

interpret ts w@(While cond c) = do
  if decide ts cond
    then do
      ts' <- interpret ts c
      interpret ts' w
    else return ts
  
interpret ts (Seq []) = return ts
interpret ts (Seq (c:cs)) = do
  ts' <- interpret ts c
  interpret ts' $ Seq cs
  
parseAep input file =
	case parse aep file input of
		Left e -> error $ show e
		Right ast -> ast 

		
main = do
  args <- getArgs
  if length args /= 1
    then error "Specify one input file."
    else do
      let fileName = args!!0
      input <- readFile fileName
      let ast = parseAep input fileName
      interpret [] ast 

--end demo.hs
