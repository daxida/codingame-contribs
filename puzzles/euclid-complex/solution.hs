import Data.Complex
import Text.Printf


fmt :: Complex Float -> String
fmt (0 :+ im)  = printf "%.0fj" im
fmt (re :+ im)
  | im > 0 = printf "(%.0f+%.0fj)" re im
  | im < 0 = printf "(%.0f-%.0fj)" re (-im)
  | otherwise = printf "(%.0f+0j)" re

closest :: Float -> Float
closest n = if abs (n - c) <= 0.5 then c else f
  where
    c = fromIntegral (ceiling n)
    f = fromIntegral (floor n)

gcd' :: Complex Float -> Complex Float -> IO (Complex Float)
gcd' z1 z2 = do
    putStrLn $ fmt z1 ++ " = " ++ fmt z2 ++ " * " ++ fmt q ++ " + " ++ fmt r
    if r == 0 then return z2 else gcd' z2 r
  where 
    (x :+ y) = z1 / z2
    q = (closest x) :+ (closest y)
    r = z1 - (z2 * q)

main :: IO ()
main = do
    input_line <- getLine
    let [xa, ya] = map read $ words input_line
        za = xa :+ ya

    input_line <- getLine
    let [xb, yb] = map read $ words input_line
        zb = xb :+ yb

    g <- gcd' za zb
    putStrLn $ "GCD(" ++ fmt za ++ ", " ++ fmt zb ++ ") = " ++ fmt g
