-- https://www.codingame.com/ide/demo/10478063e6114614ba99dc4d389049258ac06d9

import Text.Parsec
import Text.Parsec.String
import Control.Monad (replicateM)
import Data.Char (toUpper, toLower, isSpace)

-- For strings that are not inside a tag
pAny :: Parser String
pAny = (:[]) <$> anyChar

-- Parses any chars that are not followed by any of the given suffix strings
-- while not consuming the suffix strings
pTill :: [String] -> Parser String
pTill strings = manyTill anyChar $ choice $ map (try . lookAhead . string) strings

pBold :: Parser String
pBold = try $ do
  string "<<"
  fst <- pTill [">>", "[["]
  inner <- option "" $ pVar
  snd <- pTill [">>"]
  string ">>"
  let fmt = (map toUpper fst) <> inner <> (map toUpper snd)
  return $ fmt

pVar :: Parser String
pVar = try $ do
  string "[["
  fst <- pTill ["]]", "<<"]
  inner <- option "" $ pBold
  snd <- pTill ["]]"]
  string "]]"
  let fmt = (map toLower fst) <> inner <> (map toLower snd)
  return $ fmt

p :: Parser String
p = do 
  matches <- many $ pBold <|> pVar <|> pAny
  return $ concat matches

trim :: String -> String
trim [] = []
trim (x:xs)
  | isSpace x = if null xs || isSpace (head xs) then trim xs else x : trim xs
  | otherwise = x : trim xs

strip :: String -> String
strip = f . f
   where f = reverse . dropWhile isSpace

apply :: Parser String -> String -> IO ()
apply p test = case parse p "" test of
  Left err -> do
    putStrLn "No match"
    print err
  Right result -> putStrLn $ strip $ trim $ result

main :: IO ()
main = do
  n <- readLn
  tests <- replicateM n getLine
  let results = map (apply p) tests
  sequence_ results
