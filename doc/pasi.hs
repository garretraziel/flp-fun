module Main( main ) where

import System.Environment( getArgs )
--import System.IO

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

-- #####################################################################
-- ####################### DATOVE TYPY #################################
-- #####################################################################

--Datovy typ pro jmena funkci
--hlavni program je Main
--ostatni funkce jsou Func jmeno
data Function = Main
  | Func String
    deriving (Show, Eq)
  
--Typy se kterymi interpret pracuje
data PTypes = PTypeString
  | PTypeInt
  | PTypeDouble
    deriving (Show, Eq)

--datovy typ predstavujici jednotlive prikazy
data Command = Program [ Command ] [ Command ] Command
  | DeclVar String PTypes
  | DeclFunc Function [ Command ] PTypes
  | DefFunc Function [ Command ] PTypes [ Command ] Command
  | Assign String Expr
  | Print Expr
  | Scan String
  | Seq [ Command ]
  | If BoolExpr Command Command
  | While BoolExpr Command 
	deriving (Show, Eq)
	
--datovy typ predstavujici jednotlive vyrazy
data Expr = ConstI Int
  | ConstD Double
  | ConstS String
  | NotInit
  | Var String
  | UnaryMinus Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | FuncCall Function [ Expr ]
  deriving (Show, Eq)

--datovy typ predstavujici boolovske vyrazy = porovnani vyrazu
data BoolExpr = Equal Expr Expr
  | NotEqual Expr Expr 
  | Greater Expr Expr
  | Lower Expr Expr
  | NotLower Expr Expr
  | NotGreater Expr Expr
	deriving (Show, Eq)
	
--datovy typ predstavujici tabulku symbolu
--u kazde promenne je zaznamenan nazev, typ a hodnota
type SymbolTable = [(String, PTypes, Expr)]

--datovy typ predstavujici tabulku funkci
--u kazde funkce je zaznamenan nazev (typ Function), typy promennych
--a prikaz predstavujici definici, ci deklaraci funkce
type FunctionTable = [(Function, [ PTypes ], Command)]
	

-- #####################################################################
-- #################### LEXIKALNI ANALYZA ##############################
-- #####################################################################

-- ################## DEFINICE POMOCNYCH FUNKCI ########################
aelDef = emptyDef
	{ commentStart   = "{"
	, commentEnd     = "}"
	, nestedComments = False
	, identStart     = letter <|> char '_'
	, identLetter    = alphaNum <|> char '_'
	, opStart        = oneOf "=+*<>-:"
	, opLetter       = oneOf "=+<>*-"
	, reservedOpNames= [ ":=", "=", "+", "*", "div", "<", ">", "<>", "-" ]
	, reservedNames  = [ ":=", "begin", "div", "do", "double", "else", "end", "function", "if", "integer", "readln", "string", "then", "var", "while", "writeln" ]
	, caseSensitive  = True
	}

lexer = P.makeTokenParser aelDef

whiteSpace= P.whiteSpace lexer
natural   = P.natural lexer
float     = P.float lexer
parens    = P.parens lexer
semi      = P.semi lexer
dot       = P.dot lexer
comma     = P.comma lexer
colon     = P.colon lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer

-- ######################### VSTUP DO LEXIKALNI ANALYZY ################

--parsovani vstupniho souboru a vytvoreni ast
parseAep :: [Char] -> SourceName -> Command	
parseAep input file =
	case parse aep file input of
		Left e -> error $ show e
		Right ast -> ast 

--vstup do lexikalni analyzy
--funkce vraci abstraktni syntakticky strom
aep = do
  whiteSpace
  ast <- progr
  eof
  return ast
  <?> "aep"
  
--progr postupne nacte deklarace promennych, deklarace a definice
--funkci a nakonec sekvenci prikazu uzavrenych mezi begin a end.
progr = do
  dVar <- many declLine
  dFunc <- many declAndDefFunc
  reserved "begin"
  c <- sepBy cmd semi
  reserved "end"
  _ <- dot
  return $ Program (unfold dVar) dFunc (Seq c)
  
--vstupem unfold je pole poli
--vystupem je sjednoceni vsech podpoli vstupu
unfold :: [[a]] -> [a]
unfold x = unfold' [] x where
  unfold' y [] = y
  unfold' y (z:zs) = z ++ (unfold' y zs)


-- ################## DEKLARACE PROMENNYCH #############################
  
--decLine nacita jeden radek s deklaraci promennych
--ten je vzdy uvozen slovem var a nasleduji jednotlive
--definice identifikator : typ oddelene carkami a radek je zakoncen
--strednikem
declLine = do
	reserved "var"
	dVars <- sepBy declVar comma
	_ <- semi
	return dVars

--declVar nacita deklaraci jedne promenne: identifikator : typ
declVar = do
    i <- identifier
    _ <- colon
    pType <- pTypes
    return $ DeclVar i pType
  <?> "declVar"

--funkce nacita typ a vraci PTypes
pTypes = do
    reserved "string"
    return PTypeString
  <|> do
    reserved "integer"
    return PTypeInt
  <|> do
    reserved "double"
    return PTypeDouble

-- ######## DEKLARACE A DEFINICE FUNKCI ################################

--rozhodnuti, jestli nactem definici, ci promennou
declAndDefFunc = try declFunc
  <|> defFunc
  <?> "declAndDefFunc"

--declFunc nacita deklaraci funkce
--na konci se kontroluje, jestli nenasleduje deklarace promennych
--pripadne prikaz. V takovem pripade by se jednalo o definici
declFunc = do
    reserved "function"
    name <- identifier
    params <- parens $ sepBy declVar comma
    _ <- colon
    pType <- pTypes
    _ <- semi
    notFollowedBy declLine
    notFollowedBy cmd
    return $ DeclFunc (Func name) params pType

--defFunc nacita definici funkce. Telo funkce je dle zadani jeden prikaz
--muze byt slozeny, z tohoto duvodu musi deklarace lokalnich promennych
--zacinat slovem 'var'. Je povolen pouze jeden radek deklaraci promennych
--a ten je volitelny
defFunc = do
    reserved "function"
    name <- identifier
    params <- parens $ sepBy declVar comma
    _ <- colon
    pType <- pTypes
    _ <- semi
    localVars <- (option [] declLine)
    c <- cmd
    return $ DefFunc (Func name) params pType localVars c
    

-- ######################## NACITANI PRIKAZU ###########################
    
--nacitani jednotlivych prikazu
cmd = do
    reserved "writeln"
    e <- parens $ expr
    return $ Print e
  <|> do
    reserved "readln"
    i <- parens $ identifier
    return $ Scan i
  <|> do
    i <- identifier
    reserved ":=" <|> reservedOp ":="
    e <- expr
    return $ Assign i e
  <|> do
    reserved "if"
    b <- boolExpr
    whiteSpace
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
    reserved "begin"
    s <- sepBy cmd semi
    reserved "end"
    return $ Seq s
  <?> "command"

-- ########################## NACITANI VYRAZU ##########################

--nacitani vyrazu a podvyrazu
expr = buildExpressionParser operators term where    
    operators = [
        [ op "*" Mult, op "div" Div ],
        [ op "+" Add, op "-" Sub ]
      ]
    op name fun =
      Infix ( do { reservedOp name; return fun } ) AssocLeft

--pred zpracovanim termu je potreba zpracovat unarni minus
--tech muze byt nekolik. Pokud vstup nezacina '-' nacte se samotny term
term = do
    whiteSpace
    _ <- char '-'
    whiteSpace
    t <- term
    return $ UnaryMinus t
  <|> do
    whiteSpace
    t <- term'
    whiteSpace
    return t


--nacitani jednotlivych termu
--je treba zkouset jestli se jedna o double, integer, promennou, ci volani
--ty zacinaji stejne
term' = try double
  <|> do
    i <- natural
    return $ ConstI $ fromInteger i
  <|> do
    s <- between (char '\'') (char '\'') (many isStringChar)
    whiteSpace
    return $ ConstS s
  <|> try pFuncCall
  <|> do
    v <- identifier
    return $ Var v
  <|> parens expr
  <?> "term"      
      
--funkce zjistuje, jestli vstupni znak je, ci neni jednoducha uvozovka
notApostrof :: Char -> Bool
notApostrof '\'' = False
notApostrof _    = True 

--funkce pro nacitani znaku stringu
--pokud je na vstupu '' prevede se do retezce na '
--jinak nacita znak (ktery neni uvozovka)
isStringChar = do {try (string "''"); return '\''}
  <|> satisfy notApostrof

--nacteni cisla typu double
double = do
  d <- float
  return $ ConstD d
  
--nacteni volani funkce
pFuncCall = do
    v <- identifier
    e <- parens (sepBy expr comma)
    return $ FuncCall (Func v) e
  
-- ############### NACITANI BOOLOVSKYCH VYRAZU #########################

--nacitani jednotlivych boolovskych vyrazu
--jsou nacteny dva vyrazy a mezi nimi porovnavaci operator
boolExpr = do
    e1 <- expr
    o <- relOp
    e2 <- expr
    return $ o e1 e2
  <?> "boolean expression"
  where
    relOp = ro' "==" Equal
      <|> ro' "<>" NotEqual
      <|> ro' ">" Greater
      <|> ro' "<=" NotGreater
      <|> ro' "<" Lower
      <|> ro' ">=" NotLower
      <?> "relational operator"
    ro' name fun = do
      reservedOp name
      return fun

-- #####################################################################
-- ################## PRACE S VYRAZAMA #################################
-- #####################################################################

-- ################## PRACE S TABULKOU SYMBOLU #########################

--set inicializuje jednotlive promenne neni pak pouzit na meneni hodnoty
--vstup: Tabulka symbolu -> nazev promenne -> typ promenne -> hodnota
--vystup: nova tabulka symbolu
set :: SymbolTable -> String -> PTypes -> Expr -> SymbolTable
set [] var typ val = [(var, typ, val)]
set ( t@(v, _, _) : ts) var typ val = 
  if v == var
    then error $ "Variable " ++ var ++ " redefined"
    else t : (set ts var typ val)

--change je pouzivano pro nastavovani nove hodnoty promenne
--vstup: Tabulka symbolu -> nazev promenne -> nova hodnota
--vystup: nova tabulka symbolu
change :: SymbolTable -> String -> Expr -> SymbolTable
change [] _ _= error "Not found"
change (s@(var, typ, _):ss) v val =
    if v == var
        then if typ == (getType val)
            then (var, typ, val):ss
            else error "Not compatible types"
        else s : change ss v val

--get vraci hodnotu promenne z tabulky symbolu
--vstup: Tabulka symbolu, nazev promenne
--vystup: Hodnota promenne
get :: SymbolTable -> String -> Expr		
get [] _ = error "Not found"
get ((var, _, val):ss) v =
	if v == var
		then if val == NotInit
		  then error ("Variable " ++ (show var) ++ " not initialized")
		  else val
		else get ss v

-- ###################### VYHODNOCENI VYRAZU ###########################


--nasledujici funkce provadeji operace nad konstantami
--rozlisuji typ promenne a podle toho vraci vysledek
--automaticky pretypovavaji int na double, kde je treba
--vstup: konstanta -> konstanta
--vystup: vysledek operace, pripadne chyba, pokud operace neni definovana
--        na vstupu
soucet :: Expr -> Expr -> Expr
soucet (ConstS s1) (ConstS s2) = ConstS $ s1 ++ s2
soucet (ConstI i1) (ConstI i2) = ConstI $ i1 + i2
soucet (ConstD d) (ConstI i) = ConstD $ d + (fromIntegral i)
soucet (ConstI i) (ConstD d) = ConstD $ (fromIntegral i) + d
soucet (ConstD d1) (ConstD d2) = ConstD $ d1 + d2
soucet _ _ = error "Can't add string to num"

rozdil :: Expr -> Expr -> Expr
rozdil (ConstI i1) (ConstI i2) = ConstI $ i1 - i2
rozdil (ConstD d) (ConstI i) = ConstD $ d - (fromIntegral i)
rozdil (ConstI i) (ConstD d) = ConstD $ (fromIntegral i) - d
rozdil (ConstD d1) (ConstD d2) = ConstD $ d1 - d2
rozdil _ _ = error "Can't substract (from) string"

krat :: Expr -> Expr -> Expr
krat (ConstI i1) (ConstI i2) = ConstI $ i1 * i2
krat (ConstD d) (ConstI i) = ConstD $ d * (fromIntegral i)
krat (ConstI i) (ConstD d) = ConstD $ (fromIntegral i) * d
krat (ConstD d1) (ConstD d2) = ConstD $ d1 * d2
krat _ _ = error "Can't multiply by/with string"

divide :: Expr -> Expr -> Expr
divide (ConstI i1) (ConstI i2) = ConstI $ div i1 i2
divide _ _ = error "Only two integers can be divided"

--evaluate vyhodnocuje vyrazy
--vstup: tabulka symbolu -> tabulka funkci -> aktualni funkce -> vyraz na vyhodnoceni
--vystup: dvojice: prvni prvek - upravena tabulka symbolu (kvuli volani funkci)
--                 dryhy -- vysledek vyhodnoceni
evaluate :: SymbolTable -> FunctionTable -> Function -> Expr -> IO (SymbolTable, Expr)

evaluate ts _ _ (ConstI i) = return $ (ts, (ConstI i))
evaluate ts _ _ (ConstD d) = return $ (ts, (ConstD d))
evaluate ts _ _ (ConstS s) = return $ (ts, (ConstS s))

evaluate ts fs n (UnaryMinus e) = do
  eval <- evaluate ts fs n e
  return $ ((fst eval), rozdil (ConstI 0) (snd eval))

evaluate ts fs n (Add a b) = do
  e1 <- evaluate ts fs n a
  e2 <- evaluate (fst e1) fs n b
  return $ ((fst e2), soucet (snd e1) (snd e2))
evaluate ts fs n (Sub a b) = do
  e1 <- evaluate ts fs n a
  e2 <- evaluate (fst e1) fs n b
  return $ ((fst e2), rozdil (snd e1) (snd e2))
evaluate ts fs n (Mult a b) = do
  e1 <- evaluate ts fs n a
  e2 <- evaluate (fst e1) fs n b
  return $ ((fst e2), krat (snd e1) (snd e2))
evaluate ts fs n (Div a b) = do
  e1 <- evaluate ts fs n a
  e2 <- evaluate (fst e1) fs n b
  return $ ((fst e2), divide (snd e1) (snd e2))
evaluate ts _ _ (Var v) = return $ (ts, get ts v)
--zpracovani volani funkce
--do tabulky symbolu se pridaji lokalni promenne volane funkce a zaroven
--se zapamatuji promenne volajici funkce
--provede se telo funkce
--vybere se vracena hodnota
--odeberou se lokalni promenne volane funkce a vrati se prom. volajici
--vrati se upravena tabulka symbolu a vysledek volane funkce
evaluate ts fs n (FuncCall s e) = 
  if funcDeclRightly fs n s e
    then do
      func <- getFunction fs s
      ts' <- addLocalAndParams ts fs n e func
      c <- getCommand func
      ts'' <- interpret (snd ts') fs s c
      rV <- retVal (head ts'')
      ts''' <- return $ leaveHeadN ts'' (length ts'' - (length ts - length (fst ts')))
      return ((fst ts') ++ ts''', rV)
    else error $ show s ++ ": Wrong function call"


-- #################### ZPRACOVANI VOLANI FUNKCE #######################

--funkce kontroluje, jestli je volani spravne, jestli byla volana funkce
--deklarovana driv nez volajici a pokud sedi pocet parametru
--vstup: Tabulka funkci -> nazev volajici funkce -> nazev volane funkce -> pole parametru volani
--vystup: true - vse v poradku, jinak false
funcDeclRightly :: FunctionTable -> Function -> Function -> [ Expr ] -> Bool
funcDeclRightly fs Main s e = rightNumberOfParams fs s e
funcDeclRightly fs name s e =
  if declBefore fs s name
    then 
      rightNumberOfParams fs s e      
    else False
    
--declBefore zjistuje, jestli byla jedna funkce deklarovana (definovana)
--pred jinou, pripadne, jedna-li se o stejnou funkci
--vstup: Tabulka funkci -> nazev funkce -> nazev funkce
--vystup: true pokud byla funkce v prvnim parametru deklarovana (definovana) prvni
--        jinak false
declBefore :: FunctionTable -> Function -> Function -> Bool
declBefore [] _ _ = error "Function not defined"
declBefore ( (name, _, func) : cs ) n1 n2 = 
  if (name == n1) || (n1 == n2)
    then True
    else
      if (name == n2) && (isDefinition func)
        then False
        else declBefore cs n1 n2

--zjistuje jestli je prikaz definici
--vstup: prikaz definice ci deklarace funkce
--vystup: true - vstupem je definice funkce
--        false - jinak
isDefinition :: Command -> Bool
isDefinition (DefFunc _ _ _ _ _) = True
isDefinition _ = False
       
--kontrola, jestli je pocet parametru volani shodny s definovanym
--vstup: Tabulka funkci -> nazev funkce -> pole parametru volani
--vystup: true - pocet sedi, jinak false
rightNumberOfParams :: FunctionTable -> Function -> [ Expr ] -> Bool
rightNumberOfParams ( (name, params, _) : cs ) s e =
  if (name == s)
    then
      if ((length params) == (length e))
       then True
       else False
    else rightNumberOfParams cs s e

--funkce vraci definici funkce zadaneho jmena
--vstup: Tabulka funkci -> nazev hledane funkce
--vystup: prikaz predstavujici definici funkce
--        chyba pokud nebyla funkce definovana
getFunction :: FunctionTable -> Function -> IO Command
getFunction [] (Func s) = error $ "Function " ++ s ++ " not defined"
getFunction ( (name, _, f@(DefFunc _ _ _ _ _)) : cs) n =
  if name == n
    then return f
    else getFunction cs n
getFunction (_:cs) n = getFunction cs n

--funkce se stara o vytvoreni nove tabulky symbolu pro volanou funkci
--navaze na zacatek sve promenne, ty pak nasleduji globalni promenne
--vraci pak lokalni tabulku symbolu pro volajici funkci a novou tabulku pro volani
--vstup: Tabulka symbolu -> tabulka funkci -> jmeno volajici funkce -> parametry volani -> definice volane funkce
--vstup: dvojice: prvni prvek je lokalni tabulka symbolu lokalni funkce
--                druhy je pak nova tabulka symbolu, pro volani fce
addLocalAndParams :: SymbolTable -> FunctionTable -> Function -> [ Expr ] -> Command -> IO (SymbolTable, SymbolTable)
addLocalAndParams ts fs n p (DefFunc (Func name) par typ locDecl _) = do
  paramOfThis <- return $ numberOfLocalFunc fs n
  globalTs <- return $ leaveHeadN ts paramOfThis
  localTs <- return $ take paramOfThis ts
  params <- init_ts [] par
  tsLocal <- tieParams ts fs n (reverse params) p
  funcVar <- return [(name, typ, NotInit)]
  ts' <- init_ts (funcVar ++ tsLocal) locDecl
  return $ (localTs, ts' ++ globalTs)
  
--funkce vraci pocet promennych, ktere potrebuje volana funkce dodeklarovat
--jedna se o sve jmeno pro vystup, parametry, lokalni promenne
--vstup: tabulka funkci -> nazev funkce
--vystup: pocet promennych, ktere funkce pouziva za behu
numberOfLocalFunc :: FunctionTable -> Function -> Int
numberOfLocalFunc _ Main = 0
numberOfLocalFunc ( (_, _, DefFunc name params _ locals _) : cs) n =
  if n == name
    then (length params) + (length locals) + 1
    else numberOfLocalFunc cs n
numberOfLocalFunc ( _ : cs ) n = numberOfLocalFunc cs n

--funkce vynecha prvnich n prvku pole
--vstup: pole -> pocet prvku k vynechani
--vystup: zbytek pole
leaveHeadN :: [a] -> Int -> [a]
leaveHeadN ts 0 = ts
leaveHeadN (_ : ts) n = leaveHeadN ts (n-1)


--vytvoreni tabulky symbolu pro parametry, vola se hodnotou
--vstup: Tabulka symbolu -> tabulka funkci -> nazev volajici funkce -> tabulka symbolu predstavujici parametry -> skutecne parametry (vyrazy)
--vystup: Tabulka symbolu s nastavenymi parametry
tieParams :: SymbolTable -> FunctionTable -> Function -> SymbolTable -> [ Expr ] -> IO SymbolTable
tieParams _ _ _ [] [] = return []
tieParams ts fs n ( v@(name, t, _) : cs ) (e : es) = do
  eval <- evaluate ts fs n e
  ev <- if (getType (snd eval)) == PTypeInt && t == PTypeDouble
    then return $ intToDouble (snd eval)
    else return (snd eval)
  param <- return $ change [v] name ev
  other <- tieParams (fst eval) fs n cs es
  return $ param ++ other
  
--getType vraci typ konstanty
--vstup: Konstanta
--vystup: typ konstanty
getType :: Expr -> PTypes
getType (ConstD _) = PTypeDouble
getType (ConstI _) = PTypeInt
getType (ConstS _) = PTypeString
  
--prevod celociselne konstanty na desetinnou
--vstup: konstanta typu integer
--vystup: konstanta typu double, pripadne chyba pri spatnem vstupu    
intToDouble :: Expr -> Expr
intToDouble (ConstI i) = ConstD (fromIntegral i)
intToDouble _ = error "Not compatible type"

--funkce vraci prikaz, ktery provadi funkce
getCommand :: Command -> IO Command
getCommand (DefFunc _ _ _ _ c) = return c

--funkce vrati hodnotu z prvku tabulky symbolu
retVal :: (String, PTypes, Expr) -> IO Expr
retVal (_, _, val) = return val

      
-- ################### VYHODNOCENI BOOLOVSKYCH VYRAZU ##################

--nasledujici funkce vyhodnocuji porovnani vyrazu
--rozlisuji typ promenne a podle toho vraci vysledek
--automaticky pretypovavaji int na double, kde je treba
--vstup: konstanta -> konstanta
--vystup: vysledek operace, pripadne chyba, pokud operace neni definovana
--        na vstupu
rovno :: Expr -> Expr -> Bool
rovno (ConstS s1) (ConstS s2) = s1 == s2
rovno (ConstI i1) (ConstI i2) = i1 == i2
rovno (ConstI i) (ConstD d) =  (fromIntegral i) == d
rovno (ConstD d) (ConstI i) = d == (fromIntegral i)
rovno (ConstD d1) (ConstD d2) = d1 == d2
rovno _ _ = error "Can't compare string with number"

vetsi :: Expr -> Expr -> Bool
vetsi (ConstS s1) (ConstS s2) = s1 > s2
vetsi (ConstI i1) (ConstI i2) = i1 > i2
vetsi (ConstI i) (ConstD d) = (fromIntegral i) > d
vetsi (ConstD d) (ConstI i) = d > (fromIntegral i)
vetsi (ConstD d1) (ConstD d2) = d1 > d2
vetsi _ _ = error "Can't compare string with number"

nerovno :: Expr -> Expr -> Bool
nerovno (ConstS s1) (ConstS s2) = s1 /= s2
nerovno (ConstI i1) (ConstI i2) = i1 /= i2
nerovno (ConstI i) (ConstD d) = (fromIntegral i) /= d
nerovno (ConstD d) (ConstI i) = d /= (fromIntegral i)
nerovno (ConstD d1) (ConstD d2) = d1 /= d2
nerovno _ _ = error "Can't compare string with number"

vetsirovno :: Expr -> Expr -> Bool
vetsirovno (ConstS s1) (ConstS s2) = s1 >= s2
vetsirovno (ConstI i1) (ConstI i2) = i1 >= i2
vetsirovno (ConstI i) (ConstD d) = (fromIntegral i) >= d
vetsirovno (ConstD d) (ConstI i) = d >= (fromIntegral i)
vetsirovno (ConstD d1) (ConstD d2) = d1 >= d2
vetsirovno _ _ = error "Can't compare string with number"

mensi :: Expr -> Expr -> Bool
mensi (ConstS s1) (ConstS s2) = s1 < s2
mensi (ConstI i1) (ConstI i2) = i1 < i2
mensi (ConstI i) (ConstD d) = (fromIntegral i) < d
mensi (ConstD d) (ConstI i) = d < (fromIntegral i)
mensi (ConstD d1) (ConstD d2) = d1 < d2
mensi _ _ = error "Can't compare string with number"

mensirovno :: Expr -> Expr -> Bool
mensirovno (ConstS s1) (ConstS s2) = s1 <= s2
mensirovno (ConstI i1) (ConstI i2) = i1 <= i2
mensirovno (ConstI i) (ConstD d) = (fromIntegral i) <= d
mensirovno (ConstD d) (ConstI i) = d <= (fromIntegral i)
mensirovno (ConstD d1) (ConstD d2) = d1 <= d2
mensirovno _ _ = error "Can't compare string with number"

--funkce na vyhodnoceni porovnani
--vyhodnoti oba vyrazy a vysledek porovna, vraci pak potencionalne upravenou
--tabulku symbolu a vysledek porovnani
--vstup: Tabulka symbolu -> tabulka funkci -> nazev akt. funkce -> boolovsky vyraz
--vystup: dvojice - nova tabulka symbolu a vysledek porovnani
decide :: SymbolTable -> FunctionTable -> Function -> BoolExpr -> IO (SymbolTable, Bool)
decide ts fs n (Equal a b) = do
  e1 <- evaluate ts fs n a
  e2 <- evaluate (fst e1) fs n b
  return $ ((fst e2), rovno (snd e1) (snd e2))
decide ts fs n (NotEqual a b) = do
  e1 <- evaluate ts fs n a
  e2 <- evaluate (fst e1) fs n b
  return $ ((fst e2), nerovno (snd e1) (snd e2))
decide ts fs n (Greater a b) = do
  e1 <- evaluate ts fs n a
  e2 <- evaluate (fst e1) fs n b
  return $ ((fst e2), vetsi (snd e1) (snd e2))
decide ts fs n (NotGreater a b) = do
  e1 <- evaluate ts fs n a
  e2 <- evaluate (fst e1) fs n b
  return $ ((fst e2), mensirovno (snd e1) (snd e2))
decide ts fs n (Lower a b) = do
  e1 <- evaluate ts fs n a
  e2 <- evaluate (fst e1) fs n b
  return $ ((fst e2), mensi (snd e1) (snd e2))
decide ts fs n (NotLower a b) = do
  e1 <- evaluate ts fs n a
  e2 <- evaluate (fst e1) fs n b
  return $ ((fst e2), vetsirovno (snd e1) (snd e2))

-- #####################################################################
-- ##################### INTERPRETACE ##################################
-- #####################################################################


-- ######################### POMOCNE FUNKCE ############################
--funkce vypise danou konstantu podle typu
vypis :: Expr -> IO ()
vypis (ConstS s) = putStrLn s
vypis (ConstI i) = putStrLn $ show i
vypis (ConstD d) = putStrLn $ show d

--vstup: pole prikazu typu DeclVar
--vystup: typy deklarovanych promennych
typesOf :: [ Command ] -> [ PTypes ]
typesOf [] = []
typesOf ( (DeclVar _ typ) : cs) = typ : typesOf cs


--funkce vraci typ promenne
--vstup: Tabulka symbolu -> nazev promenne
--vystup: typ promenne	
getTypeVar :: SymbolTable -> String -> PTypes
getTypeVar [] _ = error "Not fond"
getTypeVar ((var, t, _):ss) v = 
    if v == var
        then t
        else getTypeVar ss v

--isPossible kontroluje, jestli neni funkce redefinovana, ci redeklarovana
--zjistuje tez pripadne chyby, jako rozdilny pocet parametru a pod
--vstup: pole prikazu typu defFunc a declFunc
--vystup: true pokud je vse v poradku, jinak false   
isPossible :: [ Command ] -> Command -> Bool
isPossible [] _ = True
isPossible ( (DefFunc fun@(Func name) _ _ _ _) : cs) d@(DefFunc i _ _ _ _) =
  if fun == i
  then error $ "Function " ++ name ++ " redefined"
  else isPossible cs d
isPossible ( (DeclFunc fun@(Func name) _ _) : cs) d@(DeclFunc i _ _) =
  if fun == i
  then error $ "Function " ++ name ++ " declared twice"
  else isPossible cs d
isPossible ( (DefFunc fun@(Func name) params typ _ _) : cs) d@(DeclFunc i p t) =
  if fun == i
  then
    if (params == p) && (typ == t)
      then isPossible cs d
      else error $ "Definition and declaration of function " ++ name ++ " doesn't match."
  else isPossible cs d
isPossible ( (DeclFunc fun@(Func name) params typ) : cs) d@(DefFunc i p t _ _) =
  if fun == i
  then 
    if (params == p) && (typ == t)
      then isPossible cs d
      else error $ "Definition and declaration of function " ++ name ++ " doesn't match."
  else isPossible cs d

--kontrola jestli nazev funkce nekoliduje s globalni promennou
--vstup: tabulka symbolu -> nazev
--vystup: true : jmeno jiz pouzito
--        false jmeno lze pouzit
colidingNames :: SymbolTable -> String -> Bool
colidingNames [] _ = False
colidingNames ( (n, _, _) : ts) name = 
  if n == name
    then True
    else colidingNames ts name

-- ####################### INICIALIZACE STRUKTUR #######################

--funkce inicalizuje tabulku symbolu
init_ts :: SymbolTable -> [ Command ] -> IO SymbolTable
init_ts ts c = return $ init_ts' ts c where
  init_ts' :: SymbolTable -> [ Command ] -> SymbolTable
  init_ts' ts' [] = ts'
  init_ts' ts' ( (DeclVar s t) :cs) = set (init_ts' ts' cs) s t NotInit 



--vytvoreni tabulky funkci z prikazu defFunc a declFunc
--kontroluje pokud je pridani mozne, jinak vraci chybu
--udrzuje poradi, aby bylo mozne zjistit, kdy bylo co deklarovano
init_fs :: SymbolTable -> [ Command ] -> IO FunctionTable
init_fs ts c = return $ init_fs' ts c where
  init_fs' _ [] = []
  init_fs' ts' ( d@(DeclFunc fun@(Func name) params _) :cs) =
    if colidingNames ts' name
      then error $ "Name " ++ name ++ " overloaded"
      else
        if isPossible cs d
          then (fun, (typesOf params),  d) : init_fs' ts' cs
          else error $ "Error in processing declaration of function " ++ name
  init_fs' ts' ( d@(DefFunc fun@(Func name) params _ _ _) : cs) = 
    if colidingNames ts' name
      then error $ "Name " ++ name ++ " overloaded"
      else
        if isPossible cs d
          then (fun, (typesOf params), d) : init_fs' ts' cs
          else  error $ "Error in processing definition of function " ++ name

-- ########################## SAMOTNY INTERPRET ########################

--interpret provadi jednotlive prikazy
--vstup: Tabulka symbolu -> tabulka funkci ->  nazev aktualni funkce -> prikaz k provedeni
--vystup: upravena tabulka symbolu   
interpret :: SymbolTable -> FunctionTable -> Function -> Command -> IO SymbolTable

--program nejdrive vytvori tabulku symbolu a funkci a pak zacne provadet prikazy
interpret [] [] name (Program vars funcs cmds) = do
  ts <- init_ts [] vars
  fs <- init_fs ts funcs
  interpret ts fs name cmds
  
interpret ts _ _ (Seq []) = return ts
interpret ts fs name (Seq (c:cs)) = do
  ts' <- interpret ts fs name c
  interpret ts' fs name $ Seq cs
  
interpret ts fs name (Assign v e) = do
  eval <- evaluate ts fs name e
  return $ change (fst eval) v (snd eval)
  
interpret ts fs name (Print e) = do
  eval <- evaluate ts fs name e
  vypis $ (snd eval)
  return (fst eval)
  
interpret ts _ _ (Scan v) = do
  case getTypeVar ts v of
    PTypeString -> do
      s <- getLine
      return $ change ts v (ConstS s)
    PTypeInt -> do
      i <- readLn :: IO Int
      return $ change ts v (ConstI i)
    PTypeDouble -> do
      d <- readLn :: IO Double
      return $ change ts v (ConstD d)
  
interpret ts fs name (If cond c1 c2) = do
  condition <- (decide ts fs name cond)
  if (snd condition)
    then interpret (fst condition) fs name c1
    else interpret (fst condition) fs name c2
    
interpret ts fs name w@(While cond c) = do
  condition <- (decide ts fs name cond)
  if (snd condition)
    then do
      ts' <- interpret (fst condition) fs name c
      interpret ts' fs name w
    else
      return ts

-- #####################################################################
-- ############################ MAIN ###################################
-- #####################################################################

--hlavni funkce, nacte soubor, vytvori ast, interpretuje
main :: IO SymbolTable		
main = do
  args <- getArgs
  if length args /= 1
    then error "Specify one input file."
    else do
      let fileName = args!!0
      input <- readFile fileName
      let ast = parseAep input fileName  
      interpret [] [] (Main) ast 
