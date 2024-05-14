import Control.Monad
import Data.List (intercalate, maximumBy)
import Data.Function (on)
import Text.Parsec
import Text.Parsec.String

toAminoAcid :: String -> String
toAminoAcid s =
  case s of
    "UUU" -> "F"
    "CUU" -> "L"
    "AUU" -> "I"
    "GUU" -> "V"
    "UUC" -> "F"
    "CUC" -> "L"
    "AUC" -> "I"
    "GUC" -> "V"
    "UUA" -> "L"
    "CUA" -> "L"
    "AUA" -> "I"
    "GUA" -> "V"
    "UUG" -> "L"
    "CUG" -> "L"
    "AUG" -> "M"
    "GUG" -> "V"
    "UCU" -> "S"
    "CCU" -> "P"
    "ACU" -> "T"
    "GCU" -> "A"
    "UCC" -> "S"
    "CCC" -> "P"
    "ACC" -> "T"
    "GCC" -> "A"
    "UCA" -> "S"
    "CCA" -> "P"
    "ACA" -> "T"
    "GCA" -> "A"
    "UCG" -> "S"
    "CCG" -> "P"
    "ACG" -> "T"
    "GCG" -> "A"
    "UAU" -> "Y"
    "CAU" -> "H"
    "AAU" -> "N"
    "GAU" -> "D"
    "UAC" -> "Y"
    "CAC" -> "H"
    "AAC" -> "N"
    "GAC" -> "D"
    "UAA" -> "Stop"
    "CAA" -> "Q"
    "AAA" -> "K"
    "GAA" -> "E"
    "UAG" -> "Stop"
    "CAG" -> "Q"
    "AAG" -> "K"
    "GAG" -> "E"
    "UGU" -> "C"
    "CGU" -> "R"
    "AGU" -> "S"
    "GGU" -> "G"
    "UGC" -> "C"
    "CGC" -> "R"
    "AGC" -> "S"
    "GGC" -> "G"
    "UGA" -> "Stop"
    "CGA" -> "R"
    "AGA" -> "R"
    "GGA" -> "G"
    "UGG" -> "W"
    "CGG" -> "R"
    "AGG" -> "R"
    "GGG" -> "G"
    _ -> error "Invalid codon"

startCodon :: String
startCodon = "AUG"

endCodons :: [String]
endCodons = ["UAA", "UAG", "UGA"]

pTriplet :: Parser String
pTriplet = count 3 anyChar

pStartCodon :: Parser String
pStartCodon = string startCodon

pNotStartCodon :: Parser String
pNotStartCodon = try $ do
  triplet <- try pTriplet
  guard $ triplet /= startCodon
  return triplet

pEndCodon :: Parser String
pEndCodon = do
  triplet <- pTriplet
  guard $ triplet `elem` endCodons
  return triplet

pNotEndCodon :: Parser String
pNotEndCodon = try $ do
  triplet <- try pTriplet
  guard $ triplet `notElem` endCodons
  return triplet

pSequence :: Parser String
pSequence = try $ do
  many pNotStartCodon
  start <- pStartCodon
  nonEnds <- many pNotEndCodon
  end <- pEndCodon
  return $ concatMap toAminoAcid $ start : nonEnds

pSequences :: Parser [String]
pSequences = many $ try pSequence

solve :: String -> [String]
solve rna = 
  case parse pSequences "" rna of
    Left err -> []
    Right result -> result

main :: IO ()
main = do
  n <- read <$> getLine
  replicateM n $ do
    rna <- getLine
    let rnas = [rna, (drop 1 rna), (drop 2 rna)]

    -- mapM_ (\rna -> do
    --   putStrLn rna
    --   case parse pSequences "" rna of
    --     Left err -> putStrLn $ "Parse error: " ++ show err
    --     Right result -> do
    --       let seqs = intercalate "-" result
    --       putStrLn $ "Parsed seqs: " ++ seqs
    --   putStrLn "==="
    --   ) rnas
    -- putStrLn "=========="

    let seqs = map solve rnas
    let ans = maximumBy (compare `on` length . concat) seqs
    putStrLn $ intercalate "-" ans
  return ()
