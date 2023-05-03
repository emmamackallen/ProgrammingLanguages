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

cbn :: Comb -> Env -> Comb
cbn (Var x) env  = Var x
cbn (Abs f y) env = Abs f (cbn y env)
cbn (App x y) env = App (cbn x env) (cbn y env)
cbn (App (Abs f y) x) env = cbn (subst (f, x) y) env
cbn (Name x) env = cbn (lookUp x env) env 


lookUp :: CName -> Env -> Comb
lookUp n [] = error $ "Cannot find function " ++ n
lookUp n (x:xs) | (n == fst x) = snd x
                      | otherwise = lookUp n xs

subst :: (Vars, Comb) -> Comb -> Comb
subst (x, t) (Var v) | v == x = t 
                     | otherwise = Var v
subst (x, t) (App s1 s2) = subst (x, t) s1 `App` subst (x,t) s2
subst (x, t) (Abs y r) | not (elem y (fv t)) = Abs y (subst (x,t) r)
                       | otherwise = let z = freshVar (x : y : fv t)
                                         r' = subst (y, Var z) r
                                     in Abs z (subst (x,t) r') 

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

main :: IO ()
main = do
  putStrLn $ "Input file name: \n"
  fileName <- getLine
  file <- readFile fileName
  let fileLines = lines file

  let lexed = map lexer fileLines
  let env = map parser lexed 

  putStrLn $ "Enter your definition: \n"
  newDef <- getLine
  let lexedDef = lexer newDef
  let parseDef = parser2 lexedDef

  let redux = cbn parseDef env
  putStr $ "Reduced function: " ++ redux -- ?
