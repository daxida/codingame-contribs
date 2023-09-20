import Control.Applicative
import Data.Char

-- Based on the arithmetic parser in "Programming in Haskell" by G. Hutton
-- https://www.cs.nott.ac.uk/~pszgmh/pih.html

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                            []         -> []
                            [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of 
                            []         -> []
                            [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of 
                            []         -> []
                            [(v, out)] -> parse (f v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                            []         -> parse q inp
                            [(v, out)] -> [(v, out)])

---------- Primitives

item :: Parser Char
item = P (\inp -> case inp of 
                    [] -> []
                    (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

letter :: Parser Char
letter = sat isAlpha

char :: Char -> Parser Char
char c = sat (== c)

---------- Handling space

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

symbol :: String -> Parser String
symbol xs = token (string xs)

------------ Main parser

expr :: Parser String
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t ++ " + " ++ e)
           <|> return t

term :: Parser String
term = do f <- factor
          do symbol "."
             t <- term
             return (t ++ " |> " ++ f)
           <|> return f

factor :: Parser String
factor = do pure <$> letter
          <|> do symbol "("
                 e <- expr
                 symbol ")"
                 return ("(" ++ e ++")")

eval :: String -> String
eval xs = case parse expr xs of
              [(n, [])]  -> n
              [(_, out)] -> error ("Unused input " ++ out)
              []         -> error "Invalid input"

main :: IO ()
main = do
    function <- getLine
    putStrLn $ eval function