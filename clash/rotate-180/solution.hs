trans :: Char -> Char
trans c
  | c == 'b' = 'q' | c == 'q' = 'b'
  | c == 'u' = 'n' | c == 'n' = 'u'
  | c == 'p' = 'd' | c == 'd' = 'p'
  | otherwise = c

solve :: String -> String
solve = reverse . map trans

main :: IO ()
main = interact $ solve