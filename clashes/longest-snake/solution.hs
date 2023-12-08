import Data.List.Split (splitOneOf)

fmt :: [String] -> [String]
fmt iptLines = [ if odd i then x else reverse x
               | (x, i) <- zip iptLines [0..]
               ]

main :: IO ()
main = do
    clean <- concat . fmt . lines <$> getContents
    let chunks = splitOneOf "<o>" clean
    print $ (+2) $ maximum $ map length $ chunks