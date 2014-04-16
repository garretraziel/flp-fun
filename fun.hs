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

whiteSpace = P.whiteSpace lexer
integer = P.integer lexer
double = P.float lexer
stringLiteral = P.stringLiteral lexer
parens = P.parens lexer
braces = P.braces lexer
semi = P.semi lexer
comma = P.comma lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

data MultiValue = UndefInt
     | UndefStr
     | UndefDouble
     | IntegerValue Integer
     | DoubleValue Double
     | StringValue String
     deriving (Show, Eq)

data Command = DefineVar String MultiValue
     | Assign String Expr
     | Print Expr
     | Scan String
     | Seq [ Command ]
     | If Expr Command Command
     | While Expr Command  -- ditto
     | Return Expr
     | Function String [ Command ] Command
     | MainF Command
     | Eval Expr
     | Declare String [ Command ]
     deriving Show

data Expr = ConstInt Integer
     | ConstDouble Double
     | ConstString String
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
     | Call String [ Expr ]
     deriving Show

type VariableTable = [(String,MultiValue)]
type GlobalTable = VariableTable
type LocalTable = [VariableTable]
type SymbolTable = (GlobalTable,LocalTable,MultiValue)

type StAndValue = (SymbolTable,MultiValue)

fst' (g,_,_) = g
snd' (_,l,_) = l
thr' (_,_,r) = r

expr vars = buildExpressionParser operators (term vars) where
  operators = [
      [ op "*" Mult, op "/" Div ],
      [ op "+" Add, op "-" Sub ],
      [ op "<" Lesser, op "<=" LesserOrEqual, op ">" Greater, op ">=" GreaterOrEqual,
        op "==" Equal, op "!=" NotEqual]
    ]
  op name fun =
    Infix ( do { reservedOp name; return fun } ) AssocLeft

term vars = do
    i <- integer
    return $ ConstInt $ fromInteger i
  <|> do
    d <- double
    return $ ConstDouble d
  <|> do
    s <- stringLiteral
    return $ ConstString s
  <|> do
    try funcCallParser
  <|> do
    v <- identifier
    if not (isMember v vars) then error ("Use of undeclared variable " ++ (show v))
    else do
      return $ Var v
  <|> parens (expr vars)
  <?> "term"
  where
    funcCallParser = do
      f <- identifier
      exprs <- parens $ sepBy (expr vars) comma
      return $ Call f exprs

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

cmd vars = do
    reserved "print"
    e <- parens $ expr vars
    semi
    return $ Print e
    <|> do
    reserved "scan"
    i <- parens $ identifier
    semi
    return $ Scan i
    <|> do
    try identifierParser
    <|> do
    reserved "if"
    b <- parens $ expr vars
    seq1 <- braces $ many $ cmd vars
    reserved "else"
    seq2 <- braces $ many $ cmd vars
    return $ If b (Seq seq1) (Seq seq2)
    <|> do
    reserved "while"
    b <- parens $ expr vars
    seq <- braces $ many $ cmd vars
    return $ While b (Seq seq)
    <|> do
    reserved "return"
    e <- expr vars
    semi
    return $ Return e
    <|> do
    e <- expr vars
    semi
    return $ Eval e
    <?> "command"
    where
      identifierParser = do
        i <- identifier
        reserved "="
        e <- expr vars
        semi
        return $ Assign i e

-- toto jeste zmenit
funcBody args = do
    vars <- many $ varDeclarationLine
    if not (checkVars (vars++args)) then error "Duplicate definition of variables"
    else do
         seq <- many $ cmd (vars ++ args)
         return $ Seq (vars++seq) -- TODO: toto mozna bude stacit takto?

checkVars :: [Command] -> Bool
checkVars [] = True
checkVars (x:xs) = checkVars' x xs
          where
            checkVars' (DefineVar _ _) [] = True
            checkVars' a@(DefineVar name1 _) (b@(DefineVar name2 _):xs) =
              if name1 == name2 then False
              else (checkVars' a xs) && (checkVars' b xs)

isMember :: String -> [Command] -> Bool
isMember _ [] = False
isMember name1 ((DefineVar name2 _):xs) =
         if name1 == name2 then True
         else isMember name1 xs

-- zatim jen int funkce, poresit nadtyp typu (podobne jak je v pasi.hs to PTypes)
funcDeclaration = do
  reserved "int"
  i <- identifier
  vars <- parens $ sepBy varDeclarationType comma
  semi  -- nebo _ <- semi ?? kdo vi
  return $ Declare i vars
  <?> "function definition"

funcDefinition globals = do
  reserved "int"
  i <- identifier
  vars <- parens $ sepBy varDeclarationType comma
  seq <- braces $ funcBody (vars++globals)
  if i /= "main"
  then return $ Function i vars seq
  else if (length vars) /= 0 then error "Main function cannot have arguments"
       else return $ MainF seq
  <?> "function declaration"

funcAST globals = do
  -- jestli chapu dobre, try je dobrej k look ahead
  try funcDeclaration
  <|>
  funcDefinition globals
  <?> "function"

aep = do
    whiteSpace
    globals <- many $ try varDeclarationLine
    if not (checkVars globals) then error "Multiple definition of same global variable"
    else do
    asts <- many $ funcAST globals
    -- main <- mainAST
    eof
    return (globals, asts)
    <?> "aep"

getFunction :: [Command] -> String -> Command
getFunction [] _ = error "Cannot find called function"
getFunction (f@(Function name1 _ _) : asts) name2 =
        if name1 == name2 then f
        else getFunction asts name2
getFunction (_:asts) name2 = getFunction asts name2

getFuncArgs :: [Command] -> String -> [Command]
getFuncArgs [] _ = error "Cannot find called function"
getFuncArgs ((Function name1 args _) : asts) name2 =
        if name1 == name2 then args
        else getFuncArgs asts name2
getFuncArgs (_:asts) name2 = getFuncArgs asts name2

getSt :: SymbolTable -> String -> MultiValue
getSt ([], ([]:_), _) variable = error $ "Variable \"" ++ variable ++ "\" not in scope"
getSt (global, (((name, value):xs):rest), r) variable =
  if variable == name then value
  else getSt (global, (xs:rest), r) variable
getSt (((name, value):xs), rest, r) variable =
  if variable == name then value
  else getSt (xs, rest, r) variable

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
setSt (global, local@(head:rest), r) variable value =
      case setVariableInList head variable value of
           Nothing -> case setVariableInList global variable value of
                           Nothing -> error $ "Variable \"" ++ variable ++ "\" not in scope"
                           Just result -> (result, local, r)
           Just result -> (global, (result:rest), r)

-- TODO: mozna nejaka kontrola neexistence?
insertStLocal :: SymbolTable -> String -> MultiValue -> SymbolTable
insertStLocal (global, (head:rest), r) name value = (global, ((name, value):head):rest, r)

-- TODO: taky prebirat funkci a rovnou vytvorit a naplnit argumenty
prepareStForCall :: SymbolTable -> [Command] -> IO SymbolTable
prepareStForCall (global, local, r) defs = do
  interpret (global, ([]:local), r) (Seq defs) []

insertStRetVal :: SymbolTable -> MultiValue -> SymbolTable
insertStRetVal (globals,locals,_) val = (globals,locals,val)

returnStFromFunction :: SymbolTable -> StAndValue
returnStFromFunction (globals, _:locals, r) = ((globals, locals, UndefInt), r)

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

-- TODO: oduvodnit do dokumentace !!!
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

eval :: SymbolTable -> Expr -> [Command] -> IO StAndValue
eval st (ConstInt i) _ = return $ (st, IntegerValue i)
eval st (ConstDouble d) _ = return $ (st, DoubleValue d)
eval st (ConstString s) _ = return $ (st, StringValue s)
eval st (Var v) _ = return $ (st, getSt st v)
eval st (Add e1 e2) fs = do
  op1 <- (eval st e1 fs)
  op2 <- (eval (fst op1) e2 fs)
  return $ (fst op2, (snd op1) `add` (snd op2))
eval st (Sub e1 e2) fs = do
  op1 <- (eval st e1 fs)
  op2 <- (eval (fst op1) e2 fs)
  return $ (fst op2, (snd op1) `sub` (snd op2))
eval st (Mult e1 e2) fs = do
  op1 <- (eval st e1 fs)
  op2 <- (eval (fst op1) e2 fs)
  return $ (fst op2, (snd op1) `mult` (snd op2))
eval st (Div e1 e2) fs = do
  op1 <- (eval st e1 fs)
  op2 <- (eval (fst op1) e2 fs)
  return $ (fst op2, (snd op1) `divide` (snd op2))
eval st (Lesser e1 e2) fs = do
  op1 <- (eval st e1 fs)
  op2 <- (eval (fst op1) e2 fs)
  return $ (fst op2, (snd op1) `lesser` (snd op2))
eval st (Greater e1 e2) fs = do
  op1 <- (eval st e1 fs)
  op2 <- (eval (fst op1) e2 fs)
  return $ (fst op2, (snd op1) `greater` (snd op2))
eval st (Equal e1 e2) fs = do
  op1 <- (eval st e1 fs)
  op2 <- (eval (fst op1) e2 fs)
  return $ (fst op2, (snd op1) `equal` (snd op2))
eval st (NotEqual e1 e2) fs = do
  op1 <- (eval st e1 fs)
  op2 <- (eval (fst op1) e2 fs)
  return $ (fst op2, notMultiVal $ (snd op1) `equal` (snd op2))
eval st (LesserOrEqual e1 e2) fs = do
  op1 <- (eval st e1 fs)
  op2 <- (eval (fst op1) e2 fs)
  return $ (fst op2, ((snd op1) `lesser` (snd op2)) `orMultiVal` ((snd op1) `equal` (snd op2)))
eval st (GreaterOrEqual e1 e2) fs = do
  op1 <- (eval st e1 fs)
  op2 <- (eval (fst op1) e2 fs)
  return $ (fst op2, ((snd op1) `greater` (snd op2)) `orMultiVal` ((snd op1) `equal` (snd op2)))
eval st (Call name vars) fs = do
--  return $ (st, IntegerValue 0)
  evaledArgs <- evalArgs st vars fs
  emptyFrameSt <- prepareStForCall (fst evaledArgs) (getFuncArgs fs name)
  let local_stack = (snd' emptyFrameSt)!!0
  framest <- fillVars emptyFrameSt (snd evaledArgs) local_stack
  newst <- interpret framest (getFunction fs name) fs
  return $ returnStFromFunction newst
  where
    evalArgs st [] fs = return $ (st, [])
    evalArgs st (x:xs) fs = do
      evaled <- eval st x fs
      rest <- evalArgs (fst evaled) xs fs
      return $ ((fst rest), ((snd evaled):(snd rest)))
    fillVars st [] [] = do return st
    fillVars st _ [] = error "Bad argument count"
    fillVars st [] _ = error "Bad argument count"
    fillVars st (val:vals) ((name, _):xs) = do
      fillVars (setSt st name val) vals xs

evaluateBool :: SymbolTable -> Expr -> [Command] -> IO (SymbolTable, Bool)
evaluateBool st expr fs = do
             res <- (eval st expr fs)
             if (snd res) /= IntegerValue 0 then return (fst res, True)
             else return (fst res, False)

scan :: SymbolTable -> String -> IO SymbolTable
scan st name = do
  nval <- scan' $ getSt st name
  return $ setSt st name nval
  where
    scan' :: MultiValue -> IO MultiValue
    scan' (IntegerValue _) = do
      i <- readLn :: IO Integer
      return $ IntegerValue i
    scan' UndefInt = do
      i <- readLn :: IO Integer
      return $ IntegerValue i
    scan' (DoubleValue _) = do
      d <- readLn :: IO Double
      return $ DoubleValue d
    scan' UndefDouble = do
      d <- readLn :: IO Double
      return $ DoubleValue d
    scan' (StringValue _) = do
      s <- getLine
      return $ StringValue s
    scan' UndefStr = do
      s <- getLine
      return $ StringValue s

-- otestovat:
-- 1. jestli je funkce deklarovana maximalne jednou (a pred definici?)
-- 2. jestli je funkce definovana prave jednou
-- 3. jestli v deklaraci a definici souhlasi pocet a typ argumentu a navratova hodnota
-- 4. jestli je kazda globalni promenna deklarovana prave jednou
--
-- na urovni funkci pak:
-- 5. jestli pouzita promenna existuje
-- 6. jestli jestli je kazda promenna deklarovana prave jednou
-- 7. jestli byla deklarovana/definovana volana funkce
-- 8. jestli soulasi pocet a typ argumentu volani funkce
-- 9. typova kontrola operaci
-- 10. typova kontrola assignu
-- 11. typova kontrola navratu funkce
-- 12. nejak resit pretypovani u volani integer -> double
--realityCheck :: [Command] -> Bool
--realityCheck [] = True
--realityCheck (head:tail) = (checkCollision head tail) && realityCheck tail
--                           where
--                             checkCollision 

--    tabulka symbolu - aktualni prikaz - tabulka funkci - vystup
interpret :: SymbolTable -> Command -> [Command] -> IO SymbolTable
interpret st (DefineVar name value) _ = do
          return $ insertStLocal st name value
interpret st (Assign name e) fs = do
          res <- eval st e fs
          return $ setSt (fst res) name (snd res)
interpret st (Print e) fs = do
          res <- eval st e fs
          showVal (snd res)
          return (fst res)
          where
            showVal (IntegerValue i) = do putStrLn $ show i
            showVal (DoubleValue d) = do putStrLn $ show d
            showVal (StringValue s) = do putStrLn s
            showVal _ = error "Trying to print undefined variable"
interpret st (Scan name) _ = do
          newst <- scan st name
          return newst
interpret st (Seq []) _ = do
          return st
interpret st (Seq (first:others)) fs = do
          if thr' st /= UndefInt then return $ st
          else turboEncabulator first others fs
          where
            turboEncabulator (Return e) _ fs = do
               res <- eval st e fs
               return $ insertStRetVal (fst res) (snd res)
            turboEncabulator command others fs = do
               newst <- interpret st first fs
               interpret newst (Seq others) fs
interpret st (If e seq1 seq2) fs = do
          res <- evaluateBool st e fs
          if (snd res) then do
                      interpret (fst res) seq1 fs
          else do
               interpret (fst res) seq2 fs
interpret st loop@(While e seq) fs = do
          res <- evaluateBool st e fs
          if (snd res) then do
                      newst <- interpret (fst res) seq fs
                      interpret newst loop fs
          else do
               return $ fst res
interpret st (MainF seq) fs = do
          interpret st seq fs
interpret st (Function _ params seq) fs = do
          interpret st seq fs
interpret st (Eval e) fs = do
          evaled <- eval st e fs
          return (fst evaled)
interpret st _ _ = do
          putStrLn "other"
          return st

parseAep input file =
         case parse aep file input of
              Left e -> error $ show e
              Right ast -> ast

main = do
     args <- getArgs
     if length args /= 1
     then error "Input file should be specified"
     else do
          let fileName = args!!0
          input <- readFile fileName
          let (globs, asts) = parseAep input fileName
          preparedSt <- prepareSt createSt globs 
          interpret preparedSt (getMain asts) asts
     where
       getMain :: [Command] -> Command
       getMain [] = error "Main function missing"
       getMain ((Function _ _ _) : asts) = getMain asts
       getMain (m@(MainF _) : _) = m
       getMain (_:asts) = getMain asts
       prepareSt st globs = do
           (g,l:_,r) <- interpret createSt (Seq globs) []
           return $ (l,[[]],r)
       createSt = ([],[[]],UndefInt)
