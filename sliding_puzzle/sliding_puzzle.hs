import Data.Vector (Vector, fromList, toList, (//), (!), elemIndex)
import qualified Data.List as List
import Data.List.Split (chunksOf)
import Data.Foldable (minimumBy)
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

data Node = Node {
                size  :: Int,        -- length of board side
                zero  :: Int,        -- position of zero
                slots :: Vector Int  -- flattened board
            }

main = interact (\x -> "gs: " ++ x)

printSolution :: Node -> IO()
printSolution start = printPath $ search start (goalFor start)

search :: Node                  -- ^ start
       -> Node                  -- ^ goal
       -> Maybe [Node]          -- ^ shortest path
search start goal = fmap (reverse . (goal :) . trackPath goal) $ astar openSet closedSet parents fScores gScores goal
    where
        openSet = Set.insert start Set.empty
        closedSet = Set.empty
        parents = Map.empty
        gScores = Map.insert start 0 Map.empty
        fScores = Map.insert start (h start goal) Map.empty


astar :: Set Node               -- ^ open set
      -> Set Node               -- ^ closed set
      -> Map Node Node          -- ^ parents
      -> Map Node Int           -- ^ fScores (cost of paths to the goal which go through each node)
      -> Map Node Int           -- ^ gScores (cost of path from start to each node)
      -> Node                   -- ^ goal

      -> Maybe (Map Node Node)  -- ^ final parents

astar openSet closedSet parents fScores gScores goal
    | current == goal           = Just parents
    | (Set.size openSet) == 0   = Nothing
    | otherwise                 = astar openSet' closedSet' parents' fScores' gScores' goal
    where
        parents' = foldr (\x -> Map.insert x current) parents usefulNeighbours
        gScores' = foldr (\x -> Map.insert x tScore) gScores usefulNeighbours
        fScores' = foldr (\x -> Map.insert x (tScore + (h x goal))) fScores usefulNeighbours
        openSet' = Set.delete current (Set.union openSet (Set.fromList newNeighbours))
        closedSet' = Set.insert current closedSet
        tScore = (Map.findWithDefault inf current gScores) + 1
        usefulNeighbours = closerNeighbours ++ newNeighbours
        closerNeighbours = filter (\x -> ((Set.member x openSet) && (tScore < (Map.findWithDefault inf x gScores)))) neighbours
        newNeighbours = filter (\x -> Set.notMember x openSet) neighbours
        neighbours = filter (\x -> Set.notMember x closedSet) (children current)
        current = minimumBy (\x y -> compare (fScores Map.! x) (fScores Map.! y)) openSet


trackPath :: Node -> Map Node Node -> [Node]
trackPath goal parents

    | Just parent   <- lookup = parent : (trackPath parent parents)
    | Nothing       <- lookup = []
    where
        lookup = Map.lookup goal parents


children :: Node -> [Node]
children Node { size=s, zero=z, slots=b } = moveUp ++ moveDown ++ moveLeft ++ moveRight
    where
        moveUp
            | s > z = []
            | otherwise = [Node { size=s, zero=z - s, slots=(b // [(z, b ! (z - s))]) }]

        moveDown
            | z >= (length b) - s = []
            | otherwise = [Node { size=s, zero=z + s, slots=(b // [(z, b ! (z + s))]) }]

        moveLeft
            | z `mod` (length b) == 0 = []
            | otherwise = [Node { size=s, zero=z - 1, slots=(b // [(z, b ! (z - 1))]) }]

        moveRight
            | z `mod` (length b) == length b - 1 = []
            | otherwise = [Node { size=s, zero=z + 1, slots=(b // [(z, b ! (z + 1))]) }]



nodeDist :: Node -> Node -> Int
nodeDist a b = boardDist (size a) (slotsWithZero a) (slotsWithZero b)

boardDist :: Int -> (Vector Int) -> (Vector Int) -> Int
boardDist len a b = sum $ map (\i -> indexDist len i (fromJust $ elemIndex (a ! i) b)) [0..(length a - 1)]

indexDist :: Integral a => a -> a -> a -> a
indexDist len a b = (abs (xa - xb)) + (abs (ya - yb))
    where
        xa = a `mod` len
        xb = b `mod` len
        ya = a `div` len
        yb = b `div` len


goalFor :: Node -> Node
goalFor node = makeNode $ [1..((size node)^2 - 1)] ++ [0]

makeNode :: [Int] -> Node
makeNode plates = Node {
    size = truncate $ sqrt $ fromIntegral $ length plates,
    zero = fromJust (List.elemIndex 0 plates),
    slots = fromList plates
}

printNode :: Node -> IO ()
printNode = putStrLn . show

printPath :: Maybe [Node] -> IO()
printPath (Just nodes)  = mapM_ putStrLn $ pathDirections nodes
printPath Nothing       = putStrLn "no solution"

instance Show Node where
    show node = "{" ++ (List.intercalate " . " $ map (List.intercalate " ") $ map (map show) $ matrix node) ++ "}"

instance Eq Node where
    (==) a b = ((size a) == (size b)) && ((slotsWithZero a) == (slotsWithZero b))

instance Ord Node where
    compare a b = compare (slotsWithZero a) (slotsWithZero b)

matrix :: Node -> [[Int]]
matrix node = chunksOf (size node) $ toList $ slotsWithZero node

slotsWithZero :: Node -> Vector Int
slotsWithZero Node {slots=board, zero=z} = board // [(z, 0)]

direction :: Node -> Node -> String
direction Node { size=s, zero=z1 } Node { zero=z2 }
    | z1 - z2 == s  = "up"
    | z2 - z1 == s  = "down"
    | z1 - z2 == 1  = "left"
    | z2 - z1 == 1  = "right"
    | otherwise     = "teleport"

pathDirections :: [Node] -> [String]
pathDirections nodes = zipWith direction (init nodes) (tail nodes)

h :: Node -> Node -> Int
h = nodeDist

inf :: Int
inf = maxBound
