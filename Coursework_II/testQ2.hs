--idea: if ray is absorbed/reflected/deflected at any point, atom exists - if no atoms at all, don't return that row/column
import Data.List (tails, intersect)
import qualified Data.Set as Set
type Pos = (Int, Int) --(Col, Row)
data Side = North | South | East | West deriving (Show, Eq, Ord)
type EdgePos = (Side, Int)
type Atoms = [Pos]
data Marking = Absorb | Reflect | Path EdgePos deriving (Show, Eq, Ord)
type Interactions = [(EdgePos, Marking)]

calcBBInteractions :: Int -> Atoms -> Interactions
calcBBInteractions n atoms = [(edgePos, simulateRay n atoms edgePos) | side <- [North, East, South, West], position <- [1..n], let edgePos = (side, position)]

--create ray and make sure it runs until 1
simulateRay :: Int -> Atoms -> EdgePos -> Marking
simulateRay n atoms (side, entry) = --starting coordinate
    case side of --starting side
        North -> simulatePath n atoms entry South (entry, 1) (0,1) --from North, going South
        South -> simulatePath n atoms entry North (entry, n) (0,-1) --from South, going North
        West -> simulatePath n atoms entry East (1, entry) (1,0) --from West, going East
        East -> simulatePath n atoms entry West (n, entry) (-1,0) --from East, going West

simulatePath :: Int -> Atoms -> Int -> Side -> Pos -> (Int, Int) -> Marking
simulatePath n atoms entry side pos@(col, row) dir@(dc, dr)
    | not (isValid n pos) = Path (side, entry) -- Invalid ray
    | pos `elem` atoms = Absorb -- Absorb the ray if coordinates match
    --check if there are any atoms next to the current ray's position - assume side represents the path it's coming from
    | (side == North || side == South) && ((col-1, row) `elem` atoms || (col+1,row) `elem` atoms) = Reflect
    | (side == East || side == West) && ((col, row-1) `elem` atoms || (col,row+1) `elem` atoms) = Reflect
    | otherwise = isDeflected n (side, entry) pos atoms dir

-- Check if a position is within the valid bounds of the grid
isValid :: Int -> Pos -> Bool
isValid n (col, row) = col >= 1 && col <= n && row >= 1 && row <= n

isEdge :: Int -> Pos -> Bool
isEdge n (c, r) = c == 0 || c == (n + 1) || r == 0 || r == (n + 1)

isDeflected :: Int -> EdgePos -> (Int, Int) -> Atoms -> (Int, Int) -> Marking
isDeflected n (side, entry) (rayCol, rayRow) atoms dir@(dc, dr)
    | isDeflectionHelper (-1, 1) = changePath n atoms (side, entry) (-1,1) (rayCol, rayRow)
    | isDeflectionHelper (1, 1) = changePath n atoms (side, entry) (1,1) (rayCol, rayRow)
    | isDeflectionHelper (-1, -1) = changePath n atoms (side, entry) (-1,-1) (rayCol, rayRow)
    | isDeflectionHelper (1, -1) = changePath n atoms (side, entry) (1,-1) (rayCol, rayRow)
    | otherwise = simulatePath n atoms entry side (rayCol+dc, rayRow+dr) dir
    where
        isDeflectionHelper (dCol, dRow) =
            (rayCol + dCol, rayRow + dRow) `elem` atoms &&
            (rayCol, rayRow + dRow) `notElem` atoms &&
            (rayCol + dCol, rayRow) `notElem` atoms

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

--Question 2--
solveBB :: Int -> Interactions -> [Atoms]
solveBB n xs = let x = getMaxGridSize 0 xs in filterAtomCombinations x (allPossibleCombinations n (buildGrid x)) xs []

--new code starting here
--find the maximum grid size based on the interactions given
getMaxGridSize :: Int -> Interactions -> Int
getMaxGridSize accumulator [] = accumulator --if interactions is empty
getMaxGridSize accumulator (((_, value), Absorb) : xs) = getMaxGridSize (max accumulator value) xs
getMaxGridSize accumulator (((_, value), Reflect) : xs) = getMaxGridSize (max accumulator value) xs
getMaxGridSize accumulator (((_, a), (Path (_, b))) : xs) = getMaxGridSize (max accumulator (max a b)) xs

--build grid using (maximum) grid size
buildGrid :: Int -> Atoms
buildGrid n = [(x,y) | x <- [1..n], y <- [1..n]]

--generate all possible combinations of atoms using maximum grid size
--implementation of tails function, from module List, e.g. tails [1,2,3] => [[1,2,3], [2,3], [3], []]
--http://zvon.org/other/haskell/Outputlist/tails_f.html
allPossibleCombinations :: Int -> Atoms -> [Atoms]
allPossibleCombinations 0 _ = [[]]
allPossibleCombinations _ [] = []
allPossibleCombinations n xs = [x : ys | (x:xs') <- tails xs, ys <- allPossibleCombinations (n-1) xs']

--better for smaller lists
--all (elem ys) xs checks if all elements in xs are present in ys.
isInteractionMatch :: Interactions -> Interactions -> Bool
isInteractionMatch xs ys = all (`elem` ys) xs

--get the combinations of atoms based on interactions given
filterAtomCombinations :: Int -> [Atoms] -> Interactions -> [Atoms] -> [Atoms]
filterAtomCombinations _ [] _ acc = acc
filterAtomCombinations n (x:xs) interactions acc =
  filterAtomCombinations n xs interactions $
    if isInteractionMatch interactions (calcBBInteractions n x)
      then x : acc
      else acc

main :: IO ()
main = do
  let interactionTest = calcBBInteractions 8 [(2,3),(4,6),(7,3),(7,8)]
  let test = solveBB 4 interactionTest
  
  let test2 = solveBB 4 [((North,1),Path (West,2)),((North,2),Absorb),((North,3),Path (North,6))]
  let testMax = getMaxGridSize 0 [((North,1),Absorb),((North,2),Absorb), ((North,3), Reflect), ((North,3), Path (West,6))]
  let smallTest = solveBB 2 [((North,1),Absorb),((North,2),Absorb)]
  print smallTest
  --print test2
