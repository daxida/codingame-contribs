import Data.Char (ord)

recur :: [Int] -> Int -> Int
recur center radius
    | null center = 1
    | otherwise = sum next
    where
        c1 : ncenter = center
        next = [recur ncenter (radius - abs (c1 - c2)) | 
                c2 <- [0..25], radius >= abs (c1 - c2)]

main :: IO ()
main = do
    radius <- readLn
    center <- map ((subtract 97) . ord) <$> getLine
    print $ recur center radius
