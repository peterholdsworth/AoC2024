{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
import Data.List

type Count = Int

parse :: String -> [(Int, Count)]
parse s = map (\z -> (z, 1)) $ map read (words s)

digits :: Int -> Int
digits n = if n < 10 then 1 else 1 + digits (div n 10)

split :: Int -> (Int, Int)
split n = (div n m, mod n m)
  where m = 10^(div (digits n) 2)

blink :: [(Int,Count)] -> [(Int,Count)]
blink [] = []
blink ((x,c):xs)
  | x == 0         =              (1,c):(blink xs)
  | odd (digits x) =       (2024 * x,c):(blink xs)
  | otherwise      =          (xl,c):(xr,c):(blink xs)
  where (xl,xr) = split x

acc :: [(Int,Count)] -> [(Int,Count)]
acc [] = []
acc [s] = [s]
acc ((x,c1):(y,c2):xs) 
  |  x == y    = acc ((x,c1+c2):xs)
  |  otherwise = (x,c1): acc ((y,c2):xs) 

gather  :: [(Int,Count)] -> [(Int,Count)]
gather = acc . sortBy (\(x,c) (y,d) -> compare x y)

blink25 :: [(Int,Count)] -> [(Int,Count)]
blink25 = head . take 1 . drop 25 . iterate blink

blink75 :: [(Int,Count)] -> [(Int,Count)]
blink75 = blink25 . gather . blink25 . gather . blink25

puzzle1 :: [(Int,Count)] -> Int 
puzzle1 = sum . map snd . blink25

puzzle2 :: [(Int,Count)] -> Int 
puzzle2 = sum . map snd . blink75

main = do 
  -- x <- readFile "input0"
  x <- readFile "input"
  let y = parse x
  print $ puzzle1 y
  print $ puzzle2 y
