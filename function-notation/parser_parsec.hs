-- The rules are no different from the usual arithmetic operations
-- where (+) becomes function addition, and (*) becomes composition.
-- Priority: Factor (Var | Par) >> Term (App) >> Expr (Add)

import Text.Parsec
import Text.Parsec.String (Parser)

data Expr = Add Expr Expr 
          | App Expr Expr 
          | Var Char 
          | Par Expr 
          deriving Show

symbol :: String -> Parser String
symbol s = spaces *> string s <* spaces

var :: Parser Expr
var = Var <$> letter

paren :: Parser Expr
paren = Par <$> between (symbol "(") (symbol ")") expr

factor :: Parser Expr
factor = var <|> paren

term :: Parser Expr
term = chainl1 factor $ try (symbol ".") >> return App

expr :: Parser Expr
expr = chainl1 term $ try (symbol "+") >> return Add

mainParser :: Parser Expr
mainParser = expr <* eof

eval :: Expr -> String
eval (Var c) = [c]
eval (Par e) = "(" ++ eval e ++ ")"
eval (Add e1 e2) = eval e1 ++ " + " ++ eval e2
eval (App e1 e2) = eval e2 ++ " |> " ++ eval e1

main :: IO ()
main = do
  input <- getLine
  case parse mainParser "" input of
    Left  err  -> print err
    Right expr -> putStrLn (eval expr)
