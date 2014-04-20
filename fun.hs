-- interpreter C-like jazyka

module Main ( main ) where

import System.Environment ( getArgs )
import System.IO

-- knihovny Parsec
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim as Pr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

---------------------- PARSOVANI A SEMANTICKA ANALYZA ----------------------
-- nastaveni promennych parseru
aelDef = emptyDef
       { commentStart = "/*"
       , commentEnd = "*/"
       , commentLine = "//"
       , nestedComments = False
       , identStart = letter <|> char '_'
       , identLetter = alphaNum <|> char '_'
       , opStart = oneOf "=+-*/<>!"
       , opLetter = opStart aelDef
       , reservedOpNames = ["=", "+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!="]
       , reservedNames = ["double", "else", "if", "int", "print", "scan", "string", "while", "return"]
       , caseSensitive = True
       }

lexer = P.makeTokenParser aelDef

-- pomocne parsery
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

-- obecny datovy typ pro ulozeni vsech potrebnych hodnot
data MultiValue = UndefInt
     | UndefStr
     | UndefDouble
     | IntegerValue Integer
     | DoubleValue Double
     | StringValue String
     deriving (Show, Eq)

-- seznam prikazu pro interpreter
data Command = DefineVar String MultiValue -- vlozeni promenne do tabulky symbolu
     | Assign String Expr -- nastaveni hodnoty v tabulce symbolu
     | Print Expr
     | Scan String
     | Seq [ Command ] -- sekvence prikazu, co se vykonavaji postupne
     | If Expr Command Command
     | While Expr Command
     | Return Expr
     -- funkce -- nazev - navratovy typ - seznam argumentu - telo funkce
     | Function String MultiValue [ Command ] Command
     | MainF Command
     -- prikaz pro vyhodnoceni vyrazu (napriklad pro volani funkce bez prirazeni)
     | Eval Expr
     -- deklarace funkce, ve skutecnosti se neinterpretuje
     | Declare String MultiValue [ Command ]
     deriving Show

-- seznam typu vyrazu
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
     | Call String [ Expr ] -- volani funkce s danymi parametry
     deriving Show

-- lokalni kontext je list dvojic, na prvni pozici nazev promenne,
-- na druhe hodnota
type VariableTable = [(String,MultiValue)]
type GlobalTable = VariableTable
-- pole lokalnich kontextu - pri volani funkce se na zacatek vlozi novy
type LocalTable = [VariableTable]
-- tabulka symbolu je tabulka globalnich promennych, tabulka lokalnich
-- a navratova hodnota posledni funkce
type SymbolTable = (GlobalTable,LocalTable,MultiValue)

type StAndValue = (SymbolTable,MultiValue)

fst' (g,_,_) = g
snd' (_,l,_) = l
thr' (_,_,r) = r

-- parser vyrazu vcene operaci
expr vars = buildExpressionParser operators (term vars) where
  operators = [
      [ op "*" Mult, op "/" Div ],
      [ op "+" Add, op "-" Sub ],
      [ op "<" Lesser, op "<=" LesserOrEqual, op ">" Greater, op ">=" GreaterOrEqual,
        op "==" Equal, op "!=" NotEqual]
    ]
  op name fun =
    Infix ( do { reservedOp name; return fun } ) AssocLeft

-- parser vyrazu
-- argument je seznam promennych pro kontrolu pouziti
term vars =
    try parseDouble
  <|> do
    i <- integer
    return $ ConstInt $ fromInteger i
  <|> do
    s <- stringLiteral
    return $ ConstString s
  <|> do
    try funcCallParser
  <|> do
    v <- identifier
    -- kontrola existence pouzite promenne
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
    parseDouble = do
      d <- double
      return $ ConstDouble d

-- parser deklaraci typu promennych
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

-- parser deklaraci na zacatku funkci - vcetne stredniku
varDeclarationLine = do
  var <- varDeclarationType
  semi
  return var
  <?> "variable declaration, one per line"           

-- parsovani prikazu
-- argument je seznam promennych pro kontrolu pouziti
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
    -- zkusi parsovat identifikator - kvuli vyrazu
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
    -- zkusi parsovat jen jako vyraz ukonceny strednikem
    e <- expr vars
    semi
    return $ Eval e
    <?> "command"
    where
      identifierParser = do
        i <- identifier
        reservedOp "="
        e <- expr vars
        semi
        return $ Assign i e

isInt :: MultiValue -> Bool
isInt UndefInt = True
isInt (IntegerValue _) = True
isInt _ = False

isDouble :: MultiValue -> Bool
isDouble UndefDouble = True
isDouble (DoubleValue _) = True
isDouble _ = False

isStr :: MultiValue -> Bool
isStr UndefStr = True
isStr (StringValue _) = True
isStr _ = False

-- zjisti, jestli jsou dve hodnoty stejneho typu
sameType :: MultiValue -> MultiValue -> Bool
sameType a b
  | (isInt a) && (isInt b) = True
  | (isDouble a) && (isDouble b) = True
  | (isStr a) && (isStr b) = True
  | otherwise = False

-- parsovani tela funkce
funcBody args globals = do
    vars <- many $ varDeclarationLine -- sekce deklarace
    -- otestuji se duplicity
    if not (checkVars (vars++args)) then error "Duplicate definition of variables"
    else do
         -- telo parsuje jako posloupnost prikazu
         -- cmd predava seznam argumentu, globalnich a lokalnich promennych
         seq <- many $ cmd (vars ++ args ++ globals)
         return $ Seq (vars++seq)

-- zkontroluje, jestli neni promenna definovana dvakrat
checkVars :: [Command] -> Bool
checkVars [] = True
checkVars (x:xs) = checkVars' x xs
          where
            checkVars' (DefineVar _ _) [] = True
            checkVars' a@(DefineVar name1 _) (b@(DefineVar name2 _):xs) =
              if name1 == name2 then False
              else (checkVars' a xs) && (checkVars' b xs)

-- vraci true, pokud pole commandu obsahuje deklaraci dane promenne
isMember :: String -> [Command] -> Bool
isMember _ [] = False
isMember name1 ((DefineVar name2 _):xs) =
         if name1 == name2 then True
         else isMember name1 xs

-- deklarace funkce
funcDeclaration = do
  retType <- typeParser
  i <- identifier
  vars <- parens $ sepBy varDeclarationType comma
  semi
  return $ Declare i retType vars
  <?> "function definition"

-- parser navratovych typu funkci
typeParser = do
  reserved "int"
  return UndefInt
  <|> do
  reserved "double"
  return UndefDouble
  <|> do
  reserved "string"
  return UndefStr
  <?> "type parsing"

-- parser definice funkce
funcDefinition globals = do
  retType <- typeParser -- typ funkce
  i <- identifier -- identifikator funkce
  vars <- parens $ sepBy varDeclarationType comma -- parser argumentu
  seq <- braces $ funcBody vars globals -- parser tela funkce
  if i /= "main"
  then return $ Function i retType vars seq
  else if (length vars) /= 0 then error "Main function cannot have arguments"
       else return $ MainF seq
  <?> "function declaration"

-- parser deklarace nebo definice funkce
funcAST globals = do
  try funcDeclaration
  <|>
  funcDefinition globals
  <?> "function"

-- hlavni parser programu
aep = do
    whiteSpace
    -- sekce globalnich promennych
    globals <- many $ try varDeclarationLine
    if not (checkVars globals) then error "Multiple definition of same global variable"
    else do
    -- pouziti manyAccum misto many, aby kazda funkce mela pristup
    -- k ostatnim, jiz definovanym nebo deklarovanym funkcim
    asts <- Pr.manyAccum checker $ funcAST globals
    eof
    return (globals, asts)
    <?> "aep"

-- otestuje, zda se v tele funkce nevola zatim nedefinovana nebo
-- nedeklarovana funkce. pouzita v manyAccum
checker f fs = if (check' f (fs++[f])) then (fs++[f])
               else error $ "Calling undefined function"
              where
                check' (Function _ _ _ cmds) fs = check' cmds fs
                check' (Assign _ e) fs = checkExpr e fs
                check' (Print e) fs = checkExpr e fs
                check' (Seq cmds) fs = all (\x -> check' x fs) cmds
                check' (If e cmds1 cmds2) fs = (checkExpr e fs) && (check' cmds1 fs) && (check' cmds2 fs)
                check' (While e cmds) fs = (checkExpr e fs) && (check' cmds fs)
                check' (Return e) fs = checkExpr e fs
                check' (MainF cmds) fs = check' cmds fs
                check' (Eval e) fs = checkExpr e fs
                check' _ _ = True
                checkExpr (Add e1 e2) fs = (checkExpr e1 fs) && (checkExpr e2 fs)
                checkExpr (Sub e1 e2) fs = (checkExpr e1 fs) && (checkExpr e2 fs)
                checkExpr (Mult e1 e2) fs = (checkExpr e1 fs) && (checkExpr e2 fs)
                checkExpr (Div e1 e2) fs = (checkExpr e1 fs) && (checkExpr e2 fs)
                checkExpr (Equal e1 e2) fs = (checkExpr e1 fs) && (checkExpr e2 fs)
                checkExpr (NotEqual e1 e2) fs = (checkExpr e1 fs) && (checkExpr e2 fs)
                checkExpr (Greater e1 e2) fs = (checkExpr e1 fs) && (checkExpr e2 fs)
                checkExpr (Lesser e1 e2) fs = (checkExpr e1 fs) && (checkExpr e2 fs)
                checkExpr (GreaterOrEqual e1 e2) fs = (checkExpr e1 fs) && (checkExpr e2 fs)
                checkExpr (LesserOrEqual e1 e2) fs = (checkExpr e1 fs) && (checkExpr e2 fs)
                checkExpr (Call name exprs) fs = (all (\x -> checkExpr x fs) exprs) && (getFunctionForCall fs name)
                checkExpr _ _ = True
                getFunctionForCall [] name = error $ "Calling undefined function " ++ name
                getFunctionForCall ((Declare name1 _ _):fs) name2 =
                  if name1 == name2 then True
                  else getFunctionForCall fs name2
                getFunctionForCall ((Function name1 _ _ _):fs) name2 =
                  if name1 == name2 then True
                  else getFunctionForCall fs name2
                getFunctionForCall (_:fs) name = getFunctionForCall fs name

---------------------- INTERPRETACE ----------------------

-- vrati funkci z tabulky funkci podle nazvy
getFunction :: [Command] -> String -> Command
getFunction [] _ = error "Cannot find called function"
getFunction (f@(Function name1 _ _ _) : asts) name2 =
        if name1 == name2 then f
        else getFunction asts name2
getFunction (_:asts) name2 = getFunction asts name2

-- vrati seznam argumentu funkce podle nazvu z tabulky funkci
getFuncArgs :: [Command] -> String -> [Command]
getFuncArgs [] _ = error "Cannot find called function"
getFuncArgs ((Function name1 _ args _) : asts) name2 =
        if name1 == name2 then args
        else getFuncArgs asts name2
getFuncArgs (_:asts) name2 = getFuncArgs asts name2

-- vrati typ zadane funkce podle nazvu z tabulky funkci
getFuncType :: [Command] -> String -> MultiValue
getFuncType [] _ = error "Cannot find called function"
getFuncType ((Function name1 fType _ _) : asts) name2 =
        if name1 == name2 then fType
        else getFuncType asts name2
getFuncType (_:asts) name2 = getFuncType asts name2

-- vrati hodnotu promenne z tabulky symbolu
getSt :: SymbolTable -> String -> MultiValue
getSt ([], ([]:_), _) variable = error $ "Variable \"" ++ variable ++ "\" not in scope"
getSt (global, (((name, value):xs):rest), r) variable =
  if variable == name then value
  else getSt (global, (xs:rest), r) variable
getSt (((name, value):xs), rest, r) variable =
  if variable == name then value
  else getSt (xs, rest, r) variable

-- zkusi nastavit hodnotu zadane promenne v tabulce. vrati Nothing, pokud ji nenajde
setVariableInList :: VariableTable -> String -> MultiValue -> Bool -> Maybe VariableTable
setVariableInList [] variable _ _ = Nothing
setVariableInList (first@(name, val):xs) variable value canChangeType =
                  if name == variable then changeValue name val value canChangeType
                  else case setVariableInList xs variable value canChangeType of
                            Nothing -> Nothing
                            Just a -> Just (first:a)
                  where
                    -- pravidla pro zmenu hodnoty. pokud je posledni argument True, dojde k pretypovani
                    -- int na double
                    changeValue :: String -> MultiValue -> MultiValue -> Bool -> Maybe VariableTable
                    changeValue name (IntegerValue _) v@(IntegerValue _) _ = Just ((name, v):xs)
                    changeValue name (UndefInt) v@(IntegerValue _) _ = Just ((name, v):xs)
                    changeValue name (DoubleValue _) v@(DoubleValue _) _ = Just ((name, v):xs)
                    changeValue name (UndefDouble) v@(DoubleValue _) _ = Just ((name, v):xs)
                    changeValue name (StringValue _) v@(StringValue _) _ = Just ((name, v):xs)
                    changeValue name (UndefStr) v@(StringValue _) _ = Just ((name, v):xs)
                    changeValue name (UndefDouble) (IntegerValue a) True = Just ((name, (DoubleValue $ fromIntegral a)):xs)
                    changeValue name (DoubleValue _) (IntegerValue a) True = Just ((name, (DoubleValue $ fromIntegral a)):xs)
                    changeValue _ _ _ _ = error $ "Cannot assing to variable " ++ name ++ ": bad type of value " ++ (show value)

-- nastavi hodnotu promenne v tabulce funkci. promenna musi existovat
setSt :: SymbolTable -> String -> MultiValue -> Bool -> SymbolTable
setSt (global, local@(head:rest), r) variable value canChangeType =
      -- zkusi nastavit lokalni promennou. pokud se to nepovede, nastavi globalni
      case setVariableInList head variable value canChangeType of
           Nothing -> case setVariableInList global variable value canChangeType of
                           Nothing -> error $ "Variable \"" ++ variable ++ "\" not in scope"
                           Just result -> (result, local, r)
           Just result -> (global, (result:rest), r)

-- vlozi promennou se zadanym nazvem na zacatek tabulky lokalnich promennych
insertStLocal :: SymbolTable -> String -> MultiValue -> SymbolTable
insertStLocal (global, (head:rest), r) name value = (global, ((name, value):head):rest, r)

-- pripravi tabulku symbolu pro volani funkce. vlozi novy kontext a interpretaci
-- definic argumentu v nem vytvori argumenty jako lokalni promenne
--
-- seznam je nutno reverzovat, protoze insertStLocal pridava na zacatek, ale
-- pri volani se nastavuji postupne od zacatku.
prepareStForCall :: SymbolTable -> [Command] -> IO SymbolTable
prepareStForCall (global, local, r) defs = do
  (g, h:l, r) <- interpret (global, ([]:local), r) (Seq defs) []
  return $ (g, (reverse h):l, r)

-- vlozi navratovou hodnotu funkce do tabulky symbolu
insertStRetVal :: SymbolTable -> MultiValue -> SymbolTable
insertStRetVal (globals,locals,_) val = (globals,locals,val)

-- vrati tabulku symbolu z volani funkce - smaze kontext a vrati navratovou hodnotu
returnStFromFunction :: SymbolTable -> IO StAndValue
returnStFromFunction (globals, _:locals, r) = do return ((globals, locals, UndefInt), r)

-- secte dve hodnoty typu MultiValue
add :: MultiValue -> MultiValue -> MultiValue
add (IntegerValue a) (IntegerValue b) = IntegerValue (a + b)
add (DoubleValue a) (DoubleValue b) = DoubleValue (a + b)
add (IntegerValue a) (DoubleValue b) = DoubleValue ((fromIntegral a) + b)
add (DoubleValue a) (IntegerValue b) = DoubleValue (a + (fromIntegral b))
add (StringValue a) (StringValue b) = StringValue (a ++ b)
add _ _ = error "Cannot add two values of uncompatible types"

-- odecte dve hodnoty typu MultiValue
sub :: MultiValue -> MultiValue -> MultiValue
sub (IntegerValue a) (IntegerValue b) = IntegerValue (a - b)
sub (DoubleValue a) (DoubleValue b) = DoubleValue (a - b)
sub (IntegerValue a) (DoubleValue b) = DoubleValue ((fromIntegral a) - b)
sub (DoubleValue a) (IntegerValue b) = DoubleValue (a - (fromIntegral b))
sub _ _ = error "Cannot substract two values of uncompatible types"

-- vynasobi dve hodnoty typu MultiValue
mult :: MultiValue -> MultiValue -> MultiValue
mult (IntegerValue a) (IntegerValue b) = IntegerValue (a * b)
mult (DoubleValue a) (DoubleValue b) = DoubleValue (a * b)
mult (IntegerValue a) (DoubleValue b) = DoubleValue ((fromIntegral a) * b)
mult (DoubleValue a) (IntegerValue b) = DoubleValue (a * (fromIntegral b))
mult _ _ = error "Cannot multiply two values of uncompatible types"

-- vydeli dve hodnoty typu MultiValue
divide :: MultiValue -> MultiValue -> MultiValue
divide (IntegerValue a) (IntegerValue b) = IntegerValue (quot a  b)
divide (DoubleValue a) (DoubleValue b) = DoubleValue (a / b)
divide (IntegerValue a) (DoubleValue b) = DoubleValue ((fromIntegral a) / b)
divide (DoubleValue a) (IntegerValue b) = DoubleValue (a / (fromIntegral b))
divide _ _ = error "Cannot divide two values of uncompatible types"

-- porovna dve hodnoty. vysledek je typu IntegerValue
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

-- logicke or implementovano nad hodnotami MultiValue
orMultiVal :: MultiValue -> MultiValue -> MultiValue
orMultiVal (IntegerValue a) (IntegerValue b) =
           if (a /= 0) || (b /= 0) then (IntegerValue 1)
           else (IntegerValue 0)

-- logicke not nad hodnotou MultiValue
notMultiVal :: MultiValue -> MultiValue
notMultiVal (IntegerValue 0) = IntegerValue 1
notMultiVal _ = IntegerValue 0

lesser = comp LT
greater = comp GT
equal = comp EQ

-- vyhodnoceni vyrazu. vraci dvojici (zmenena tabulka symbolu, navratova hodnota)
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
  -- volani funkce je taky vyraz
  -- vyhodnoti se argumenty, se kterymi se funkce vola
  evaledArgs <- evalArgs st vars fs
  -- vytvori se novy kontext, v nem se vytvori lokalni promenne pro argumenty
  emptyFrameSt <- prepareStForCall (fst evaledArgs) (getFuncArgs fs name)
  -- ziska se kontext
  let local_stack = (snd' emptyFrameSt)!!0
  -- argumenty se naplni vyhodnocenymi vyrazy
  framest <- fillVars emptyFrameSt (snd evaledArgs) local_stack
  -- interpretuje se telo funkce, to vrati zmenenou tabulku symbolu
  newst <- interpret framest (getFunction fs name) fs
  -- ziska se navratova hodnota a smaze se kontext
  r <- returnStFromFunction newst
  if not (sameType (getFuncType fs name) (snd r)) then error $ "Bad return type of function "++name
  else do return $ r
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
      fillVars (setSt st name val True) vals xs

-- vraci true, pokud vyhodnoceny vyraz odpovida logickemu true
evaluateBool :: SymbolTable -> Expr -> [Command] -> IO (SymbolTable, Bool)
evaluateBool st expr fs = do
             res <- (eval st expr fs)
             if (snd res) /= IntegerValue 0 then return (fst res, True)
             else return (fst res, False)

-- funkce nacteni hodnoty z klavesnice
scan :: SymbolTable -> String -> IO SymbolTable
scan st name = do
  nval <- scan' $ getSt st name
  return $ setSt st name nval True
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

-- kontrola deklaraci a definici funkci
-- funkci musi byt deklarovana maximalne jednou a definovana prave jednou
-- musi se rovnat pocet a typ argumentu funkce
funcCheck :: [Command] -> Bool
funcCheck fs = funcCheck' $ filter isFunc fs
  where
    isFunc (Function _ _ _ _) = True
    isFunc (Declare _ _ _) = True
    isFunc _ = False
    funcCheck' fs = (onlyOne fs) && (argsOk fs)
    onlyOne [] = True
    onlyOne ((Function name _ _ _):fs) = searchDef name fs && onlyOne fs
    onlyOne ((Declare name _ _):fs) = searchDecl name fs && onlyOne fs
    searchDef name [] = True
    searchDef name1 ((Function name2 _ _ _):fs) = if name1 /= name2 
                                                  then searchDef name1 fs
                                                  else error ("Function " ++ name1 ++ " redefined")
    searchDef name (_:fs) = searchDef name fs
    searchDecl name [] = True
    searchDecl name1 ((Declare name2 _ _):fs) = if name1 /= name2 
                                                  then searchDecl name1 fs
                                                  else error ("Function " ++ name1 ++ " declared more than once")
    searchDecl name (_:fs) = searchDecl name fs
    argsOk [] = True
    argsOk (f:fs) = (checkArgs f fs) && argsOk fs
    checkArgs _ [] = True
    checkArgs f@(Function name1 _ args1 _) ((Declare name2 _ args2):fs) = if name1 /= name2
                                                                          then checkArgs f fs
                                                                          else checkArgs' name1 args1 args2
    checkArgs f@(Declare name1 _ args1) ((Function name2 _ args2 _):fs) = if name1 /= name2
                                                                          then checkArgs f fs
                                                                          else checkArgs' name1 args1 args2
    checkArgs f (_:fs) = checkArgs f fs
    checkArgs' _ [] [] = True
    checkArgs' name [] _ = error ("Mismatched number of args in definition and declaration of function " ++ name)
    checkArgs' name _ [] = error ("Mismatched number of args in definition and declaration of function " ++ name)
    checkArgs' name ((DefineVar _ UndefInt):as1) ((DefineVar _ UndefInt):as2) = checkArgs' name as1 as2
    checkArgs' name ((DefineVar _ UndefStr):as1) ((DefineVar _ UndefStr):as2) = checkArgs' name as1 as2
    checkArgs' name ((DefineVar _ UndefDouble):as1) ((DefineVar _ UndefDouble):as2) = checkArgs' name as1 as2
    checkArgs' name (_:as1) (_:as2) = error ("Mismatched args in definition and deklaration of function " ++ name)

-- interpretace stromu prikazu
interpret :: SymbolTable -> Command -> [Command] -> IO SymbolTable
interpret st (DefineVar name value) _ = do
          return $ insertStLocal st name value
interpret st (Assign name e) fs = do
          res <- eval st e fs
          return $ setSt (fst res) name (snd res) True
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
interpret st (Seq []) _ = do -- sekvence je na konci
          return st
interpret st (Seq (first:others)) fs = do
          -- pokud se jiz vracime z nejakeho return, je definovana
          -- navratova hodnota. neprovadime tedy sekvenci dal
          if thr' st /= UndefInt then return $ st
          else turboEncabulator first others fs
          where
            -- pokud je prvni prikaz return
            turboEncabulator (Return e) _ fs = do
               res <- eval st e fs
               return $ insertStRetVal (fst res) (snd res)
            -- jinak provedu prvni prikaz a ze zbytku udelam opet sekvenci
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
interpret st (Function _ _ params seq) fs = do
          interpret st seq fs
interpret st (Eval e) fs = do
          -- mam pouze vyhodnotit vyraz. potreba kvuli
          -- volani funkci bez prirazeni
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
          if funcCheck asts
            then do
                preparedSt <- prepareSt createSt globs
                interpret preparedSt (getMain asts) asts
            else error "Error during compilation"
     where
       getMain :: [Command] -> Command
       getMain [] = error "Main function missing"
       getMain ((Function _ _ _ _) : asts) = getMain asts
       getMain (m@(MainF _) : _) = m
       getMain (_:asts) = getMain asts
       prepareSt st globs = do -- deklaruje globalni promenne
           (g,l:_,r) <- interpret createSt (Seq globs) []
           return $ (l,[[]],r)
       createSt = ([],[[]],UndefInt)
