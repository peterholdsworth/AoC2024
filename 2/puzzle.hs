{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
import Data.List

parse :: String -> [[Int]]
parse input = map (map read) arr :: [[Int]]  
  where arr = (map words . lines) input

safe :: [Int] -> Bool
safe x = rising x || falling x
  where
    rising x  =  all (== True) $ map (\(a,b) -> elem (a - b) [1..3]) $ zip x (tail x)
    falling x =  all (== True) $ map (\(a,b) -> elem (b - a) [1..3]) $ zip x (tail x)

puzzle1 :: [[Int]] -> Int
puzzle1 arr = length $ filter (==True) $ map safe arr

puzzle2 :: [[Int]] -> Int
puzzle2 arr = length $ filter (==True) $ map safesub arr
  where 
    subs b = filter (\x -> length x > length b - 2) (subsequences b)
    safesub c = any (== True) $ map safe $ subs c 
 
main = do 
  -- x <- readFile "input0"
  x <- readFile "input"
  let y = parse x
  print $ puzzle1 y
  print $ puzzle2 y
