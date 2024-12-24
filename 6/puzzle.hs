{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
import Data.List

type X = Int
type Y = Int
type Dimension = Int
type Square = (X,Y)
type Direction = (X,Y) --  v = (0,1); > = (1,0); ^ = (0,-1); < = (-1,0);
type Guard = (Square, Direction)
type Path = [Guard]
type Obstructions = [Square]
type State = (Dimension, Obstructions, Guard, Path) 

parse :: String -> State
parse s = (dimension, obstructions, (square, direction), path)
  where
    rows = lines s
    dimension = length rows
    grid = zip (concat rows) [(x, y) | y <- [1..dimension], x <- [1..dimension]]
    obstructions = map snd $ filter (((==)'#') . fst) grid
    guard = head $ filter (((/=)'#') . fst) $ filter (((/=)'.') . fst) grid
    square = snd guard
    dir x = case x of
        'v' -> ( 0, 1)
        '^' -> ( 0,-1)
        '>' -> ( 1, 0)
        '<' -> (-1, 0)
        _   -> ( 0, 0)
    direction = dir $ fst guard
    path = []

step :: State -> State
step  (dimension, obstructions, (square, direction), path) = (dimension, obstructions, (square', direction'), path')
  where 
    add (x,y) (x',y') = (x+x',y+y')
    right (x,y) = (-y, x)
    blocked = elem (add square direction) obstructions 
    square' = (if blocked then id else add direction) square 
    direction' = (if blocked then right else id) direction
    path' = if blocked then path else (square, direction):path

outOfBounds :: State -> Bool
outOfBounds (dimension, _, (square, _), _) = fst square < 1 || snd square < 1 || fst square > dimension || snd square > dimension 

finalState :: State -> State
finalState s@(_, _, guard, path)
  | outOfBounds s   = s                    -- exit grid
  | elem guard path = s                    -- loop detected
  | otherwise       = finalState $ step s  -- take next step

-- length of path to exit or 0 for loop
puzzle1 :: State -> Int 
puzzle1 s = if outOfBounds fs then length $ nub $ map fst path else 0
  where fs@(_, _, _, path) = finalState s

-- number of squares on original path that cause loops when obstructed
puzzle2 :: State -> Int
puzzle2 s@(dimension, obstructions, guard, _) = length $ filter ((==) 0) $ map puzzle1 $ map (\x -> (dimension, x:obstructions, guard, [])) $ nub $ map fst path
  where (_, _, _, path) = finalState s

main = do 
  -- x <- readFile "input0"
  x <- readFile "input"
  let y = parse x
  print $ puzzle1 y
  print $ puzzle2 y
