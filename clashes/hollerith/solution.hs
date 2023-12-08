import Data.List.Split (chunksOf)
import Data.List (intercalate)

chunkify :: String -> Int -> [String]
chunkify s n = map (\ch -> (show $ length ch) ++ "H" ++ ch) c
  where c = chunksOf n s

main :: IO ()
main = do
  n <- readLn
  s <- getLine
  putStrLn $ '/' : intercalate "," (chunkify s n) ++ "/"
