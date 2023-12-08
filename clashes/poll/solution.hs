import Control.Monad
import qualified Data.Map.Strict as Map

-- TO FINISH

type Lookup = Map.Map Int Int
type Tally = [Int] -- list of size 2

processVote :: Lookup -> Lookup -> Int -> (Int, Int, Int) -> (Lookup, Lookup)
processVote timeRecord tally timeout (userId, vote, time) =
  case Map.lookup userId timeRecord of
    Just prev ->
      if time - prev >= timeout
      then (Map.insert userId time timeRecord, Map.adjust (+1) vote tally)
      else (timeRecord, tally)
    Nothing ->
      (Map.insert userId time timeRecord, Map.adjust (+1) vote tally)

processVotes :: Int -> Int -> [(Int, Int, Int)] -> (Int, Int)
processVotes timeout n votes = (finalTally Map.! 0, finalTally Map.! 1)
  where
    (_, finalTally) =
      foldl (\(timeRec, tally) vote -> processVote timeRec tally timeout vote) (Map.empty, Map.fromList [(0, 0), (1, 0)]) votes
    
main :: IO ()
main = do
  timeout <- readLn
  n <- readLn
  votes <- replicateM n $ do
    [a, b, c] <- fmap (map read . words) getLine
    pure (a, b, c)
  let (result0, result1) = processVotes timeout n votes
  putStrLn $ show result0 ++ " " ++ show result1
