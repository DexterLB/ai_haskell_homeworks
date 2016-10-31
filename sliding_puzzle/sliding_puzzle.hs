import Data.Vector (Vector, fromList, toList, (//), (!), elemIndex)
import qualified Data.List as List
import Data.List.Split (chunksOf, splitOneOf)
import Data.Foldable (minimumBy)
import Data.Maybe (fromJust)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Data.Hashable (Hashable, hashWithSalt)

data Node = Node {
                size  :: Int,        -- length of board side
                zero  :: Int,        -- position of zero
                slots :: Vector Int  -- flattened board
            }

main = interact solve

-- | Extracts a start node from the input string and returns the directions delimited by newlines
solve :: String -> String
solve s = showPath $ search start (goalFor start)
    where start = nodeFromString s

-- | Searches for an optimal path and returns it as a list of nodes
search :: Node                  -- ^ start
       -> Node                  -- ^ goal
       -> Maybe [Node]          -- ^ shortest path
search start goal = fmap (reverse . (goal :) . trackPath goal) $ astar openSet closedSet parents endScores startScores goal
    where
        openSet = Set.insert start Set.empty
        closedSet = Set.empty
        parents = HashMap.empty
        startScores = HashMap.insert start 0 HashMap.empty
        endScores = HashMap.insert start (heuristic start goal) HashMap.empty


astar :: Set Node                   -- ^ open set
      -> Set Node                   -- ^ closed set
      -> HashMap Node Node          -- ^ parents
      -> HashMap Node Int           -- ^ endScores (cost of paths to the goal which go through each node)
      -> HashMap Node Int           -- ^ startScores (cost of path from start to each node)
      -> Node                       -- ^ goal

      -> Maybe (HashMap Node Node)  -- ^ final parents

astar openSet closedSet parents endScores startScores goal
    | (Set.size openSet) == 0   = Nothing
    | current == goal           = Just parents
    | otherwise                 = astar openSet' closedSet' parents' endScores' startScores' goal
    where
        parents' = foldr (\x -> HashMap.insert x current) parents usefulNeighbours
        startScores' = foldr (\x -> HashMap.insert x tScore) startScores usefulNeighbours
        endScores' = foldr (\x -> HashMap.insert x (tScore + (heuristic x goal))) endScores usefulNeighbours
        openSet' = Set.delete current (Set.union openSet (Set.fromList newNeighbours))
        closedSet' = Set.insert current closedSet
        tScore = (HashMap.lookupDefault inf current startScores) + 1
        usefulNeighbours = closerNeighbours ++ newNeighbours
        closerNeighbours = filter (\x -> ((Set.member x openSet) && (tScore < (HashMap.lookupDefault inf x startScores)))) neighbours
        newNeighbours = filter (\x -> Set.notMember x openSet) neighbours
        neighbours = filter (\x -> Set.notMember x closedSet) (children current)
        current = minimumBy (\x y -> compare (endScores HashMap.! x) (endScores HashMap.! y)) openSet


trackPath :: Node -> HashMap Node Node -> [Node]
trackPath goal parents

    | Just parent   <- lookup = parent : (trackPath parent parents)
    | Nothing       <- lookup = []
    where
        lookup = HashMap.lookup goal parents


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
            | z `mod` s == 0 = []
            | otherwise = [Node { size=s, zero=z - 1, slots=(b // [(z, b ! (z - 1))]) }]

        moveRight
            | z `mod` s == s - 1 = []
            | otherwise = [Node { size=s, zero=z + 1, slots=(b // [(z, b ! (z + 1))]) }]



-- | manhattan distance between two nodes
nodeDist :: Node -> Node -> Int
nodeDist a b = boardDist (size a) (slotsWithZero a) (slotsWithZero b)

-- | manhattan distance between two boards
boardDist :: Int -> (Vector Int) -> (Vector Int) -> Int
boardDist len a b = sum $ map (\i -> indexDist len i (fromJust $ elemIndex (a ! i) b)) [0..(length a - 1)]

-- | manhattan distance between coordinates
indexDist :: Integral a => a -> a -> a -> a
indexDist len a b = (abs (xa - xb)) + (abs (ya - yb))
    where
        xa = a `mod` len
        xb = b `mod` len
        ya = a `div` len
        yb = b `div` len

-- | get the goal node with the same size as the given node
goalFor :: Node -> Node
goalFor node = makeNode $ [1..((size node)^2 - 1)] ++ [0]

-- | make a node from a list of integers. 
makeNode :: [Int] -> Node
makeNode plates = Node {
    size = truncate $ sqrt $ fromIntegral $ length plates,
    zero = fromJust (List.elemIndex 0 plates),
    slots = fromList plates
}


-- | list all directions in a path, delimited by newlines
showPath :: Maybe [Node] -> String
showPath (Just nodes) = (List.intercalate "\n" $ pathDirections nodes) ++ "\n"
showPath Nothing = "no solution\n"

instance Show Node where
    show node = (List.intercalate "\n" $ map (List.intercalate " ") $ map (map show) $ matrix node) ++ "\n"

instance Eq Node where
    (==) a b = ((size a) == (size b)) && ((slotsWithZero a) == (slotsWithZero b))

instance Ord Node where
    compare a b = compare (slotsWithZero a) (slotsWithZero b)

instance Hashable Node where
    hashWithSalt salt node = hashWithSalt salt (toList $ slotsWithZero node)

-- | read a node from a string, ignoring the first number and using the rest
-- | of the numbers to form a square board. If their count is not a square
-- | number, throws an exception
nodeFromString :: String -> Node
nodeFromString s = makeNode $ map read $ tail $ filter (not . null) $ splitOneOf "\n\r " s

-- | represents a node as a square matrix
matrix :: Node -> [[Int]]
matrix node = chunksOf (size node) $ toList $ slotsWithZero node

-- | returns the node's actual slots (with a real zero at the "zero position")
slotsWithZero :: Node -> Vector Int
slotsWithZero Node {slots=board, zero=z} = board // [(z, 0)]

-- | determines the direction by which we get from one node to another
direction :: Node -> Node -> String
direction Node { size=s, zero=z1 } Node { zero=z2 }
    | z1 - z2 == s  = "up"
    | z2 - z1 == s  = "down"
    | z1 - z2 == 1  = "left"
    | z2 - z1 == 1  = "right"
    | otherwise     = "teleport"

pathDirections :: [Node] -> [String]
pathDirections nodes = zipWith direction (init nodes) (tail nodes)

heuristic :: Node -> Node -> Int
heuristic = nodeDist

inf :: Int
inf = maxBound
