type Pos = (Int, Int) --(Col, Row)
data Side = North | South | East | West deriving (Show, Eq, Ord)
type EdgePos = (Side, Int)
type Atoms = [Pos]
data Marking = Absorb | Reflect | Path EdgePos deriving (Show, Eq)
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

{-
--deflect ray and change direction accordingly
changePath :: Int -> Atoms -> EdgePos -> (Int, Int) -> (Int, Int) -> Marking
changePath n atoms (oldSide, oldEntry) (diagonalCol, diagonalRow) (rayCurCol, rayCurRow)
    | oldSide == South = if (diagonalCol,diagonalRow) == (1,1) then simulatePath n atoms rayCurRow West (rayCurCol-1, rayCurRow) (-1,0) 
                        else simulatePath n atoms rayCurRow East (rayCurCol+1, rayCurRow) (1,0)
    | oldSide == North = if (diagonalCol,diagonalRow) == (1,-1) then simulatePath n atoms rayCurRow West (rayCurCol-1, rayCurRow) (-1,0) 
                        else simulatePath n atoms rayCurRow East (rayCurCol+1, rayCurRow) (1,0)
    | oldSide == East = if (diagonalCol,diagonalRow) == (1,1) then simulatePath n atoms rayCurCol North (rayCurCol, rayCurRow-1) (0,-1) 
                        else simulatePath n atoms rayCurCol South (rayCurCol, rayCurRow+1) (0,1)
    | oldSide == West = if (diagonalCol,diagonalRow) == (-1,1) then simulatePath n atoms rayCurCol North (rayCurCol, rayCurRow-1) (0,-1) 
                        else simulatePath n atoms rayCurCol South (rayCurCol, rayCurRow+1) (0,1)
-}
main :: IO ()
main = do
    let finalTest = calcBBInteractions 8 [(2,3),(4,6),(7,3),(7,8)]
    print finalTest
    --[((North,1),Path (West,2)),((North,2),Absorb),((North,3),Path (North,6)),((North,4),Absorb),((North,5),Path (East,5)),
    --((North,6),Path (North,3)),((North,7),Absorb),((North,8),Path (East,2)),((East,1),Path (West,1)),((East,2),Path (North,8)),
    -- ((East,3),Absorb),((East,4),Path (East,7)),((East,5),Path (North,5)),((East,6),Absorb),((East,7),Path (East,4)),((East,8),Absorb),
    -- ((South,1),Path (West,4)),((South,2),Absorb),((South,3),Path (West,7)),((South,4),Absorb),((South,5),Path (West,5)),((South,6),Reflect),
    -- ((South,7),Absorb),((South,8),Reflect),((West,1),Path (East,1)),((West,2),Path (North,1)),((West,3),Absorb),((West,4),Path (South,1)),
    -- ((West,5),Path (South,5)),((West,6),Absorb),((West,7),Path (South,3)),((West,8),Absorb)]
    --let finalTest2 = calcBBInteractions 4 [(1,2),(2,2)]
    --print finalTest2
    --let finalTest3 = calcBBInteractions 4 [(2,1),(2,2)]
    --print finalTest3