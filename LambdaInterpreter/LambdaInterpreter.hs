import Data.List (nub)
import Text.Parsec
import qualified Text.Parsec.Token as L
import Text.Parsec.Language (emptyDef)



data Expr
    = Abs String Expr
    | App Expr Expr
    | Var String
    deriving Eq

instance Show Expr where
    show (Abs x e) = "\\"++x++"."++show e
    show (App (Var x) (Var y)) = x ++ " " ++ y
    show (App e@(App _ _) (Var x)) = show e ++ " " ++ x 
    show (App (Var x) e) = x ++ " ("++ show e++")"
    show (App e (Var y)) = "("++show e ++ ") " ++ y 
    show (App e1 e2) = "("++show e1 ++ ") (" ++ show e2++")"
    show (Var x) = x
    

------ Variáveis livres

freeVars :: Expr -> [String]
freeVars (Var x)     = [x]
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Abs x e)   = filter (/= x) (freeVars e)

----- Geração de variaveis fresh (evitar captura)

freshVar :: String -> [String] -> String
freshVar x used
    | x `elem` used = freshVar (x ++ "'") used
    | otherwise     = x

----- Conversao alfa

rename :: String -> String -> Expr -> Expr
rename old new (Var x)
    | x == old  = Var new
    | otherwise = Var x

rename old new (App e1 e2) =
    App (rename old new e1) (rename old new e2)

rename old new (Abs x e)
    | x == old  = Abs new (rename old new e)
    | otherwise = Abs x (rename old new e)

----- Substituição: [x := s]e

subst :: String -> Expr -> Expr -> Expr
subst x s e@(Var y)
    | y == x    = s
    | otherwise = e

subst x s (App e1 e2) =
    App (subst x s e1) (subst x s e2)

subst x s abs@(Abs y e)
    | y == x = abs  -- variável ligada, não substitui
    | y `notElem` freeVars s =
        Abs y (subst x s e)
    | otherwise =
        -- evitar captura
        let used = freeVars e ++ freeVars s ++ [x]
            y'   = freshVar y used
            e'   = rename y y' e
        in Abs y' (subst x s e')

----- Redução beta (avaliacao normal ou preguicosa)

betaLazy :: Expr -> Maybe Expr
betaLazy (App (Abs x e1) e2) =
    Just (subst x e2 e1)

betaLazy (App e1 e2) =
    case betaLazy e1 of
        Just e1' -> Just (App e1' e2)
        Nothing  ->
            case betaLazy e2 of
                Just e2' -> Just (App e1 e2')
                Nothing  -> Nothing

betaLazy (Abs x e) =
    fmap (Abs x) (betaLazy e)

betaLazy _ = Nothing 
----- Redução beta (avaliacao estrita ou por valor)

betaStrict :: Expr -> Maybe Expr
betaStrict (App (Abs x e1) e2) =
    case betaStrict e2 of
        Just e2' -> Just (App (Abs x e1) e2')
        Nothing  -> Just (subst x e2 e1)

betaStrict (App e1 e2) =
    case betaStrict e2 of
        Just e2' -> Just (App e1 e2')
        Nothing  ->
            case betaStrict e1 of
                Just e1' -> Just (App e1' e2)
                Nothing  -> Nothing

betaStrict (Abs x e) =
    fmap (Abs x) (betaStrict e)

betaStrict _ = Nothing


----- Avaliação completa (forma normal)

eval :: Expr -> [Expr]
eval e =
    case betaLazy e of
        Just e' -> e:eval e'
        Nothing -> [e]
     
evalStrict :: Expr -> [Expr]
evalStrict e =
    case betaStrict e of
        Just e' -> e:evalStrict e'
        Nothing -> [e]
   
----- Exemplos 


test1 = Abs "g" (Abs "f" (Abs "x" (App (Var "g") (App (Var "f") (Var "x")))))
test2 = App (Abs "w" (Abs "y" (Abs "x" (App (Var "y") (App (App (Var "w") (Var "y")) (Var "x")))))) (Abs "s" (Abs "z" (App (Var "s") (Var "z"))))

test3 = "(\\w.\\y.\\x. y (w y x)) ((\\a.\\b.\\c. b (a b c)) (\\s.\\z.s z))"
test4 = "(\\w.\\y.\\x. y (w y x)) (\\s.\\z.s z)"
test5 = "(\\x.\\y.\\w.\\u.x w (y w u)) (\\s.\\z.s (s z)) (\\s.\\z.s (s (s z)))"
test6 = "(\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)) (\\s.\\z.s (s z))"
test7 = "(\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)) ((\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)) (\\s.\\z.s (s (s (s z)))))"
test8 = "(\\x.\\y.\\w.\\u.x (y w) u) (\\s.\\z.s (s z)) (\\s.\\z.s (s (s z)))"
test9 = "(\\x.\\y. x y (\\t.\\u.u)) (\\v.\\f.v) ((\\x.(x (\\t.\\u.u))(\\a.\\b.a)) (\\v.\\f.f))"

testloop = "(\\y.\\z.z) ((\\x.x x) (\\x.x x))"
testfat  = "((\\f.(\\x.f (x x))(\\x.f (x x))) (\\f.\\n.(\\x.x (\\d. (\\v.\\f.f)) (\\v.\\f.v)) n (\\s.\\z.s z) ((\\x.\\y.\\w.\\u.x (y w) u) n (f ((\\n.\\f.\\x.n (\\g.\\h.h (g f)) (\\u.x) (\\u.u)) n))))) (\\s.\\z.s (s z))"


------ Lexico

lingDef = emptyDef
          { L.commentStart = "{-"
           ,L.commentEnd   = "-}"
           ,L.commentLine  = "--"
           ,L.identStart   = letter
           ,L.identLetter  = letter
          }  

lexical = L.makeTokenParser lingDef

symbol     = L.symbol lexical
parens     = L.parens lexical
identifier = L.identifier lexical  

----- Parser 
parseExpr = runParser expr [] "Lambda-calculus"

expr :: Parsec String u Expr
expr = chainl1 parserNonApp $ return $ App

var = do {i <- identifier; return (Var i)}

lambda = do symbol "\\"
            i <- identifier
            symbol "."
            e <- expr
            return (Abs i e)

parserNonApp = parens expr     -- (E)
              <|> lambda       -- \x.E
              <|> var          -- x

----------------------------------------
parserLambdaS s = case parseExpr s of
                     Left er -> print er
                     Right e -> printEval (evalStrict e)
                     
parserLambdaL s = case parseExpr s of
                     Left er -> print er
                     Right e -> printEval (eval e)

----------------------------------------

printEval [] = putStr ""
printEval (e:es) = do print e
                      printEval es

                      

main = do putStr "Lambda:"
          e <- getLine
          parserLambdaL e
                   
