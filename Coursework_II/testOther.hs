main = do 
  let atoms = [(2,3), (4,6), (7,8), (7,3)]
  let gridSize = 8
  --let i = calcBBInteractions gridSize atoms
  --putStrLn ("Interactions list with atoms at position "++show (atoms)++": \n"++ show (i)++"\n\n")
  --putStrLn ("Solving BlackBox with the following interaction "++show (i)++"... \n")
  let test = solveBB 4 [((North,1),Path (West,2)),((North,2),Absorb),((North,3),Path (North,6)),((North,4),Absorb),((North,5),Path (East,5)),((North,6),Path (North,3)),((North,7),Absorb),((North,8),Path (East,2)),((East,1),Path (West,1)),((East,2),Path (North,8)),((East,3),Absorb),((East,4),Path (East,7)),((East,5),Path (North,5)),((East,6),Absorb),((East,7),Path (East,4)),((East,8),Absorb),((South,1),Path (West,4)),((South,2),Absorb),((South,3),Path (West,7)),((South,4),Absorb),((South,5),Path (West,5)),((South,6),Reflect),((South,7),Absorb),((South,8),Reflect),((West,1),Path (East,1)),((West,2),Path (North,1)),((West,3),Absorb),((West,4),Path (South,1)),((West,5),Path (South,5)),((West,6),Absorb),((West,7),Path (South,3)),((West,8),Absorb)]
  putStrLn ("Possible positions of atoms are: "++show (test))

-- Challenge 1: Calculate All Interaction [50 Marks]
-- some basic setups
data Side = North | East | South | West | All deriving (Show, Eq, Ord) -- representing entry/exit points to the grid
data Marking = Absorb | Reflect | Path EdgePos deriving (Show, Eq)
type Pos = (Int, Int) -- representing (col, row)
type EdgePos = (Side, Int) 
type Atoms = [Pos] -- representing location of the hidden atoms
type Interactions = [(EdgePos, Marking)] -- list of outcomes of firing rays into grid
type CurrPos = (Side, Pos) -- current coor with its direction relative to an atom

-- main function
calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions n atms = [let ep = (side, x) in (ep, interaction (calcStarting ep) atms n) | side <- [North, East, South, West], x <- [1..n]]

-- generating corners for different side 
genCorners :: Side -> Atoms -> Atoms
-- (lu - left upper, ru - right upper, ll - left lower, rl - right lower)
genCorners dir atms = 
  let lu = [(x-1, y-1) | (x,y) <- atms]
      ru = [(x+1, y-1) | (x,y) <- atms]
      ll = [(x-1, y+1) | (x,y) <- atms]
      rl = [(x+1, y+1) | (x,y) <- atms] in
  if dir == North then lu ++ ru else if dir == South then ll ++ rl 
  else if dir == East then ru ++ rl else if dir == West then lu ++ ll
  else genCorners North atms ++ genCorners South atms

-- possible interferences depending the current coordinate and direction 
interferences :: CurrPos -> Atoms -> Atoms
interferences (dir, (a,b)) atms = [ (x, y) | (x,y) <- atms, 
  if dir == North then x == a && y <= b else if dir == South then x == a && y >= b 
  else if dir == East then y == b && x >= a else y == b && x <= a]

-- deflect ray and return new currPos
deflect :: Side -> Pos -> CurrPos
deflect dir (x,y)
  | dir == North = (dir, (x, y-1))
  | dir == South = (dir, (x, y+1))
  | dir == West = (dir, (x-1, y))
  | otherwise = (dir, (x+1, y))

-- deduce final EdgePos for Path given currPos
calcFinal :: CurrPos -> EdgePos
calcFinal curr@(dir, (x,y)) = 
  if dir == North || dir == South then (dir, x) else (dir, y)

-- deduce starting currPos given EdgePos
calcStarting :: EdgePos -> CurrPos
calcStarting (dir, c) =
  if dir == North then (South, (c,0)) else if dir == South then (North, (c,9))
  else if dir == East then (West, (9, c)) else (East, (0,c))

checkAbsorb :: CurrPos -> Atoms -> Bool
--absorb if atoms is the first interference met or atoms located behind the first interference met
checkAbsorb curr@(dir, c) atms =
  let i = (interferences curr atms) ++ (interferences curr (genCorners All atms)) 
      sMin = safeMin i 
      sMax = safeMax i in 
  if length i == 0 then False 
  else if dir == South || dir == East then 
    if (elem (safeMin i) atms) then True 
    else if dir == South && (elem (fst sMin, snd sMin + 1) atms) then True 
    else if dir == East && (elem (fst sMin + 1, snd sMin) atms) then True
    else False
  else if dir == North || dir == West then 
    if (elem (safeMax i) atms) then True 
    else if dir == North && (elem (fst sMax, snd sMax - 1) atms) then True 
    else if dir == West && (elem (fst sMax - 1, snd sMax) atms) then True
    else False
  else False

-- checking for reflection (when meeting any overlapping interference)
checkReflect :: CurrPos -> Atoms -> Bool
checkReflect curr@(dir, (x,y)) atms = 
  --c, i = corners and interactions based on direction
  let c = (if dir == North then (genCorners South atms) else if dir == South then (genCorners North atms) else if dir == East then (genCorners West atms) else (genCorners East atms)) 
      i = interferences curr c 
      b = checkDuplicate (minimum (i)) i in
  if (dir == North || dir == West) && b then True
  else if (dir == South || dir == East) && b then True 
  else False

-- checking for edge reflection (only need to check at starting point)
checkEdgeReflect :: CurrPos -> Atoms -> Int -> Bool
checkEdgeReflect curr@(_, (x,y)) atms gridSize = 
  if x /= 0 && y /= gridSize then False     --not at starting point, then no need to check for edge reflection
  else if length (filter (\a -> a == (x,y)) (genCorners All atms)) > 0 then True
  else False

-- deflection
deflection :: CurrPos -> Atoms -> CurrPos
deflection curr@(dir, (x,y)) atms = 
  let leftInt = (interferences curr (genCorners West atms))
      rightInt = (interferences curr (genCorners East atms))
      upInt = (interferences curr (genCorners North atms))
      lowInt = (interferences curr (genCorners South atms)) in
  if dir == North then(if safeMax leftInt > safeMax rightInt then (deflect West (safeMax leftInt)) else deflect East (safeMax rightInt))
  else if dir == South then (if safeMin leftInt < safeMin rightInt then (deflect West (safeMin leftInt)) else deflect East (safeMin rightInt))
  else if dir == West then (if safeMax upInt > safeMax lowInt then (deflect North (safeMax upInt)) else deflect South (safeMax lowInt))
  else (if safeMin upInt < safeMin lowInt then (deflect North (safeMin upInt)) else deflect South (safeMin lowInt))

-- interaction between single ray and atoms
interaction :: CurrPos -> Atoms -> Int -> Marking
interaction curr@(dir, (x,y)) atms gridSize =
  if checkAbsorb curr atms then Absorb                               
  else if checkReflect curr atms then Reflect else if checkEdgeReflect curr atms gridSize then Reflect
  else if length (interferences curr (genCorners All atms)) > 0 then (interaction (deflection curr atms) atms gridSize)  --recursion
  else Path (calcFinal curr) --terminating and return final result

--helper functions
--safe max
safeMax :: Atoms -> Pos
safeMax [] = (-1,-1)
safeMax xs = maximum xs

--safe min
safeMin :: Atoms -> Pos
safeMin [] = (2147483647, 2147483647)
safeMin xs = minimum xs

--check for duplicates
checkDuplicate :: Pos -> [Pos] -> Bool
checkDuplicate y ys = length (filter (\x -> (x == y)) ys) > 1

--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------

-- Challenge 2: Solve a Black Box [50 Marks]
-- main function to solve black box
solveBB :: Int -> Interactions -> [Atoms]
solveBB n xs = let x = findMax 0 xs in filterCombinations x (combination n (genGrid x)) xs []

-- find combination of size n from a list of elements
-- https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell
combination :: Int -> Atoms -> [Atoms]
combination 0 _ = [[]]
combination _ [] = []
combination n (x:xs) = map (x :) (combination (n-1) xs) ++ combination n xs

-- function to generate grid of size n
genGrid :: Int -> Atoms
genGrid n = [(x, y) | x <- [1..n], y <- [1..n]]

-- function that solve a black box with extra parameter for expected grid size
solveBBWithGridSize :: Int -> Int -> Interactions -> [Atoms]
solveBBWithGridSize n size xs = filterCombinations size (combination n (genGrid size)) xs []

-- check with inputted interaction list
checkInteraction :: Interactions -> Interactions -> Bool
checkInteraction [] _ = True
checkInteraction (x:xs) ys = if (elem x ys) then checkInteraction xs ys else False

-- filter a list of combination by comparing with the actual interaction input
filterCombinations :: Int -> [Atoms] -> Interactions -> [Atoms] -> [Atoms]
filterCombinations _ [] _ acc = acc
filterCombinations n (x:xs) ys acc = 
  if (checkInteraction ys (calcBBInteractions n x)) then (filterCombinations n xs ys (x: acc))
  else (filterCombinations n xs ys acc)

--function that find maximum possible grid size from a list of interaction
findMax :: Int -> Interactions -> Int
findMax acc [] = acc 
findMax acc (x:xs) = let s = snd x 
                         f = snd (fst x) in
  if (s == Absorb || s == Reflect) then (if f < acc then findMax acc xs else findMax f xs) 
  else (if f > acc || extractPath s < acc then findMax (max f (extractPath s)) xs else findMax acc xs)

-- extract value of path from Path
extractPath :: Marking -> Int
extractPath (Path (a, b)) = b