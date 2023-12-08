import Text.Printf (printf)
import Data.List (group, sort, sortOn, intercalate)

----- Draws // B wins // // A wins
data History = History Int Int Int

instance Show History where
  show (History draws bwins awins) =
    intercalate "\n" $ map ratio [awins, bwins, draws] 
    where 
      total = fromIntegral (draws + bwins + awins) :: Double
      ratio x = printf "%.2f%%" $ (100.0 * fromIntegral x) / total

getWinner :: [(Int, Int)] -> Int -> Int -> Int
getWinner [p1, p2] h w 
  | a > b = 1      -- Player B wins
  | a < b = 2      -- Player A wins
  | otherwise = 0  -- Draw
  where
    closest_a = head $ sortOn (\(y, x) -> (y, x)) [p1, p2]
    closest_b = head $ sortOn (\(y, x) -> (x, y)) [p1, p2]
    a = (snd closest_a) * h + (fst closest_a) 
    b = (fst closest_b) * w + (snd closest_b) 

exhaustive :: Int -> Int -> [Int]
exhaustive h w = map length $ group $ sort $ results 
  where 
    points = [ (y, x) 
             | y <- [0..h-1]
             , x <- [0..w-1]
             ]
    results = [ getWinner [p1, p2] h w 
              | p1 <- points
              , p2 <- points
              , p1 /= p2
              ]

main :: IO ()
main = do
  h <- readLn
  w <- readLn
  let [a, b, c] = exhaustive h w
  print $ History a b c
