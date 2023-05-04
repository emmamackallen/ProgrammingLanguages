import Data.Char
import Data.List

type CName = String
type Vars = String
type Env = [(CName, Comb)]

data Comb = Var Vars | Abs Vars Comb | App Comb Comb | Name CName
  deriving (Show,Eq)

data Token = VSym String | LPar | RPar | Dot | Backslash
            | Err String | CSym String | Eql | PC Comb
  deriving (Show,Eq)

lexer :: String -> [Token]
lexer "" = []
lexer ('(':xs) = LPar : lexer xs
lexer (')':xs) = RPar : lexer xs
lexer ('.':xs) = Dot : lexer xs
lexer ('=':xs) = Eql : lexer xs
lexer ('\\':xs) = Backslash : lexer xs
lexer (x:xs) | isSpace x = lexer xs
lexer (x:xs) | isLower x = VSym [x] : lexer xs
lexer (x:xs) | isUpper x = CSym [x] : lexer xs

parser :: [Token] -> Either (CName, Comb) String
parser s = case sr [] s of
        [PC x, Eql, PC (Name n)]  -> Left (n, x)
        [Err e] -> Right $ "Lexical error: " ++ e
        s       -> Right $ "Parse error: " ++ show s

parser2 :: [Token] -> Either Comb String
parser2 s = case sr [] s of
  [PC e]  -> Left e
  [Err e] -> Right $ "Lexical error: " ++ e
  s       -> Right $ "Parse error: " ++ show s

sr :: [Token] -> [Token] -> [Token]
sr (VSym x : s) i = sr (PC (Var x) : s) i
sr (CSym x : s) i = sr (PC (Name x) : s) i
sr (RPar : (PC x) : LPar : s) i = sr (PC x : s) i
--App e2 e1
sr (PC e2 : PC e1 : s) i = sr (PC (App e1 e2) : s) i 
--Abs PT PT
sr (PC e2: Dot : PC (Var e1) : Backslash : s) i = sr (PC (Abs e1 e2) : s) i
-- Equals
sr (PC e2 : Eql : PC (Name n) : s) i = (PC e2 : Eql : PC (Name n) : s)
--shift phase
sr s (i:is) = sr (i:s) is
sr (Err e : s) i = [Err e]
sr s [] = s

cbn :: Comb -> Env -> Comb
cbn (Var x) env  = Var x
cbn (Abs f y) env = Abs f (cbn y env)
cbn (App (Abs f y) x) env = subst (f, x) y
cbn (App x y) env = App (cbn x env) (cbn y env)
cbn (Name x) env = cbn (lookUp x env) env 

runCbn :: Comb -> Env -> Comb
runCbn c env = let d = cbn c env 
                in if (c == d) then c else runCbn d env 
            
lookUp :: CName -> Env -> Comb
lookUp n [] = error $ "Cannot find function " ++ n
lookUp n (x:xs) | (n == fst x) = snd x
                      | otherwise = lookUp n xs

subst :: (Vars, Comb) -> Comb -> Comb
subst (f, x) (Var y) | y == f = x 
                     | otherwise = Var y
subst (f, x) (App s1 s2) = subst (f, x) s1 `App` subst (f,x) s2
subst (f, x) (Abs y r) | f == y = Abs y r
                       | not (elem y (fv x)) = Abs y (subst (f,x) r)
                       | otherwise = let z = freshVar (f : y : fv x)
                                         r' = subst (y, Var z) r
                                     in Abs z (subst (f,x) r') 

fv :: Comb -> [Vars]  
fv (Var x) = [x]                                  
fv (App s t) = nub $ fv s ++ fv t
fv (Abs y r) = filter (/= y) (fv r)

-- allVars = ['a',..,'z','aa','ab',...]
allVars :: [Vars]
-- allVars = concat (map applet ("" : allVars))
allVars = "" : allVars >>= applet where
  applet s = map (\x -> s ++ [x]) ['a'..'z']

freshVar :: [Vars] -> Vars
freshVar lst = head $ filter (\x -> not (elem x lst)) allVars    

sortParser :: Either (CName, Comb) String -> (CName, Comb)
sortParser x = case x of
           Left e -> e
           Right e -> error $ "Wrong: " ++ show e 

sortParser2 :: Either Comb String -> Comb
sortParser2 x = case x of
            Left e -> e
            Right e -> error $ "Wrong: " ++ show e 

main :: IO ()
main = do
  putStrLn $ "Input file name: "
  fileName <- getLine
  file <- readFile fileName
  let fileLines = lines file

  let lexed = map lexer fileLines
  let env = map (sortParser . parser) lexed

  putStrLn $ "Enter your definition: "
  newDef <- getLine 
  let lexedDef = lexer newDef
  putStrLn (show lexedDef)
  let parseDef = sortParser2 (parser2 lexedDef)

  let redux = runCbn parseDef env
  putStrLn $ "Reduced function: " ++ show redux -- ?
