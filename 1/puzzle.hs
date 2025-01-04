{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
import Data.List

parse :: String -> ([Int], [Int])
parse input = (map (read .head) arr :: [Int], map (read . last) arr :: [Int])  
  where arr = (map words . lines) input

puzzle1 :: ([Int], [Int]) -> Int
puzzle1 (x,y) = sum $ map (\(a,b) -> abs (a - b)) $ zip (sort x) (sort y) 

puzzle2 :: ([Int], [Int]) -> Int
puzzle2 (x,y) = sum $ map fst $ [(x',y')| x' <- x, y' <- y, x' == y'] 

main = do 
  -- x <- readFile "input0"
  x <- readFile "input"
  let y = parse x
  print $ puzzle1 y
  print $ puzzle2 y
