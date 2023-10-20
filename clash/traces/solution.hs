import Control.Monad (replicateM)
import Data.List (sort, intercalate)
import Data.Char (toLower)
import Data.List.Split (splitOneOf)


solve :: String -> String -> String -> Bool
solve a b fixed =
  -- Same length and same chars
  sort a == sort b &&
  -- Same positions for fixed chars
  all (\(c1, c2) -> not (c1 `elem` fixed || c2 `elem` fixed) || c1 == c2) (zip a b) &&
  -- Same chars in "islands"
  let aa = splitOneOf fixed a
      bb = splitOneOf fixed b
  in all (\(s1, s2) -> sort s1 == sort s2) (zip aa bb)

main :: IO ()
main = do
  t <- readLn
  inputs <- replicateM t getLine
  let results = [solve a b fixed | [a, b, fixed] <- map words inputs]
  mapM_ (putStrLn . map toLower . show) results
