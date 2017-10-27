import Text.Parsec
import Type

data Expr =   Var Id
            | App Expr Expr
            | Lam Id Expr
            deriving (Eq, Show)

tiContext g i = if l /= [] then t else error ("Undefined: " ++ i ++ "\n")
   where
      l = dropWhile (\(i' :>: _) -> i /= i' ) g
      (_ :>: t) = head l

tiExpr g (Var i) = return (tiContext g i, [])
tiExpr g (App e e') = do (t, s1) <- tiExpr g e
                         (t', s2) <- tiExpr (apply s1 g) e'
                         b <- freshVar
                         let s3 = unify (apply s2 t) (t' --> b)
                         return (apply s3 b, s3 @@ s2 @@ s1)
tiExpr g (Lam i e) = do b <- freshVar
                        (t, s)  <- tiExpr (g/+/[i:>:b]) e
                        return (apply s (b --> t), s)

--- Examples ---
ex1 = Lam "f" (Lam "x" (App (Var "f") (Var "x")))
ex2 = Lam "x" (App (Var "x") (Var "x"))
ex3 = Lam "g" (Lam "f" (Lam "x" (App (Var "g") (App (Var "f") (Var "x")))))
ex4 = Lam "x" (Lam "x" (Var "x"))
ex5 = Lam "w" (Lam "y" (Lam "x" (App (Var "y") (App (App (Var "w") (Var "y")) (Var "x")))))
ex6 = Lam "x" (Lam "y" (Lam "w" (Lam "u" (App (App (Var "x") (Var "w")) (App (App (Var "y") (Var "w")) (Var "u"))))))

infer e = runTI (tiExpr [] e)

-------- Parser ---------------
parseExpr = runParser expr [] "lambda-calculus"

expr :: Parsec String u Expr
expr = chainl1 (between spaces spaces parseNonApp) $ return $ App

var = do {i <- varId; return (Var i)}

lamAbs term = do char '\\'
                 i <- varId
                 char '.'
                 e <- term
                 return (Lam i e)
			 
parseNonApp =  do {char '('; e <- expr; char ')'; return e} -- (E)
              <|> lamAbs expr                               -- \x.E
              <|> var                                       -- x

varId = many1 letter 

----------------------------------------
parseLambda s = case parseExpr s of
                     Left er -> print er
                     Right e -> (print e >> print (infer e))
			 
main = do putStr "Lambda:"
          e <- getLine
          parseLambda e		  
