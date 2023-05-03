import Data.List (sort)

rep :: String -> String
rep "0" = ""
rep x = x

solve :: Int -> Char -> [String] -> String
solve n c s
  | sort s == group = show n
  | otherwise = unwords [x | x <- group, x `notElem` s]
  where group = [replicate i c | i <- [0..n-1]]

main :: IO ()
main = do
  n <- readLn
  c <- head <$> getLine
  s <- map rep . words <$> getLine
  putStrLn $ solve n c s
