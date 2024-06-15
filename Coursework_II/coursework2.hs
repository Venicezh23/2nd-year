--idea: if ray is absorbed/reflected/deflected at any point, atom exists - if no atoms at all, don't return that row/column
import Data.List (tails, intersect)
import qualified Data.Set as Set
type Pos = (Int, Int) --(Col, Row)
data Side = North | South | East | West deriving (Show, Eq, Ord)
type EdgePos = (Side, Int)
type Atoms = [Pos]
data Marking = Absorb | Reflect | Path EdgePos deriving (Show, Eq, Ord)
type Interactions = [(EdgePos, Marking)]

--------------Question 1------------------
--main function - takes in grid size and list of atoms to generate interactions (from 1..n and side North, South, East, West)
calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions n atoms = [(edgePos, simulateRay n atoms edgePos) | side <- [North, East, South, West], position <- [1..n], let edgePos = (side, position)]

--generate ray from a specific side and entry point
simulateRay :: Int -> Atoms -> EdgePos -> Marking
simulateRay n atoms (side, entry) = --starting coordinate
    case side of --starting side
        North -> simulatePath n atoms entry South (entry, 1) (0,1) --from North, going South
        South -> simulatePath n atoms entry North (entry, n) (0,-1) --from South, going North
        West -> simulatePath n atoms entry East (1, entry) (1,0) --from West, going East
        East -> simulatePath n atoms entry West (n, entry) (-1,0) --from East, going West

--function to 'move' the ray and determine the final Marking of the ray
simulatePath :: Int -> Atoms -> Int -> Side -> Pos -> (Int, Int) -> Marking
simulatePath n atoms entry side pos@(col, row) dir@(dc, dr)
    | not (isValid n pos) = Path (side, entry) -- Invalid ray
    | pos `elem` atoms = Absorb -- Absorb the ray if coordinates match
    --check if there are any atoms next to the current ray's position - assume side represents the path it's coming from
    | (side == North || side == South) && ((col-1, row) `elem` atoms || (col+1,row) `elem` atoms) = Reflect --reflection
    | (side == East || side == West) && ((col, row-1) `elem` atoms || (col,row+1) `elem` atoms) = Reflect --reflection
    | otherwise = isDeflected n (side, entry) pos atoms dir --deflection

-- Check if a position is within the valid bounds of the grid
isValid :: Int -> Pos -> Bool
isValid n (col, row) = col >= 1 && col <= n && row >= 1 && row <= n

--checks if a ray can be deflected - if not, then will recurse back to simulatePath and increment ray accordingly
isDeflected :: Int -> EdgePos -> (Int, Int) -> Atoms -> (Int, Int) -> Marking
isDeflected n (side, entry) (rayCol, rayRow) atoms dir@(dc, dr)
    | isDeflectionHelper (-1, 1) = changePath n atoms (side, entry) (-1,1) (rayCol, rayRow)
    | isDeflectionHelper (1, 1) = changePath n atoms (side, entry) (1,1) (rayCol, rayRow)
    | isDeflectionHelper (-1, -1) = changePath n atoms (side, entry) (-1,-1) (rayCol, rayRow)
    | isDeflectionHelper (1, -1) = changePath n atoms (side, entry) (1,-1) (rayCol, rayRow)
    | otherwise = simulatePath n atoms entry side (rayCol+dc, rayRow+dr) dir
    where
        isDeflectionHelper (dCol, dRow) = --checks if atoms exist diagonally to the ray
            (rayCol + dCol, rayRow + dRow) `elem` atoms &&
            (rayCol, rayRow + dRow) `notElem` atoms &&
            (rayCol + dCol, rayRow) `notElem` atoms

--function to deflect the ray, changing its edgePos according to the diagonal hit and side it's coming from
changePath :: Int -> Atoms -> EdgePos -> (Int, Int) -> (Int, Int) -> Marking
changePath n atoms (oldSide, oldEntry) (diagonalCol, diagonalRow) (rayCurCol, rayCurRow) =
    simulatePath n atoms newEntry newSide newPos newDir
    where
        (newEntry, newSide, newPos, newDir) = case (oldSide, diagonalCol, diagonalRow) of
            (South, 1, 1)  -> (rayCurRow, West, (rayCurCol - 1, rayCurRow), (-1, 0))
            (South, _, _)  -> (rayCurRow, East, (rayCurCol + 1, rayCurRow), (1, 0))
            (North, 1, -1) -> (rayCurRow, West, (rayCurCol - 1, rayCurRow), (-1, 0))
            (North, _, _)  -> (rayCurRow, East, (rayCurCol + 1, rayCurRow), (1, 0))
            (East, 1, 1)   -> (rayCurCol, North, (rayCurCol, rayCurRow - 1), (0, -1))
            (East, _, _)   -> (rayCurCol, South, (rayCurCol, rayCurRow + 1), (0, 1))
            (West, -1, 1)  -> (rayCurCol, North, (rayCurCol, rayCurRow - 1), (0, -1))
            (West, _, _)   -> (rayCurCol, South, (rayCurCol, rayCurRow + 1), (0, 1))

-----------------Question 2-----------------
--main function - gets the possible atom positions
solveBB :: Int -> Interactions -> [Atoms]
solveBB n xs = --takes in number of atoms to find and list of interactions
    let x = getMaxGridSize 0 xs --gets maximum grid size from interactions
    in filterAtomCombinations x (allPossibleCombinations n (buildGrid x)) xs [] --build atom list

--find the maximum grid size based on the interactions given
getMaxGridSize :: Int -> Interactions -> Int
getMaxGridSize accumulator [] = accumulator --if interactions is empty
getMaxGridSize accumulator (((_, value), Absorb) : xs) = getMaxGridSize (max accumulator value) xs
getMaxGridSize accumulator (((_, value), Reflect) : xs) = getMaxGridSize (max accumulator value) xs
getMaxGridSize accumulator (((_, a), (Path (_, b))) : xs) = getMaxGridSize (max accumulator (max a b)) xs

--build grid using (maximum) grid size
buildGrid :: Int -> Atoms
buildGrid n = [(x,y) | x <- [1..n], y <- [1..n]]

--generate all possible combinations of atoms using maximum number of atoms to find
--implementation of tails function, from module List, e.g. tails [1,2,3] => [[1,2,3], [2,3], [3], []]
--http://zvon.org/other/haskell/Outputlist/tails_f.html
allPossibleCombinations :: Int -> Atoms -> [Atoms]
allPossibleCombinations 0 _ = [[]] --if number of atoms to find are zero
allPossibleCombinations _ [] = [] --if list of interactions is empty list
allPossibleCombinations n xs = [x : ys | (x:xs') <- tails xs, ys <- allPossibleCombinations (n-1) xs']

--better for smaller lists
--all (elem ys) xs checks if all elements in xs are present in ys.
isInteractionMatch :: Interactions -> Interactions -> Bool
isInteractionMatch xs ys = all (`elem` ys) xs

--get the combinations of atoms based on generated grid and interactions given
filterAtomCombinations :: Int -> [Atoms] -> Interactions -> [Atoms] -> [Atoms]
filterAtomCombinations _ [] _ acc = acc --when the generated grid is empty
filterAtomCombinations n (x:xs) interactions acc =
  filterAtomCombinations n xs interactions $ --recursive call
    if isInteractionMatch interactions (calcBBInteractions n x)
      then x : acc --if input interaction matches with generated interaction
      else acc --don't add onto accumulated atom list

main :: IO ()
main = do
  let interactionTest = calcBBInteractions 8 [(2,3),(4,6),(7,3),(7,8)]
  let test = solveBB 4 [((North,1),Path (West,2)),((North,2),Absorb),((North,3),Path (North,6)),
                        ((North,4),Absorb),((North,5),Path (East,5)),((North,6),Path (North,3)),
                        ((North,7),Absorb),((North,8),Path (East,2)),((East,1),Path (West,1)),
                        ((East,2),Path (North,8)),((East,3),Absorb)]
  print interactionTest
  --print test
  