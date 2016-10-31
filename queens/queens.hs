import Data.Vector (Vector, toList, (!), (//))
import Data.List (intercalate, tails)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import qualified Data.Vector as Vec

data Point = Point Int Int

data Board = Board (Vector Int)


main :: IO()
main = do
    n <- readLn
    putStrLn =<< (show <$> (solve n))

solve :: Int -> IO Board
solve n = optimise =<< (initialBoard n)

optimise :: Board -> IO Board
optimise board
    | conflicts board == 0 = pure board
    | otherwise = optimise =<< (optimiseRandomQueen board)

optimiseRandomQueen :: Board -> IO Board
optimiseRandomQueen board = (optimiseQueen board) =<< (randomRIO (0, (size board) - 1))

-- | move the queen with the given index to a position where she causes a minimal
-- | number of conflicts
optimiseQueen :: Board -> Int -> IO Board
optimiseQueen (Board board) queen = randMinBy conflicts $ map moveAt [0..(length board) - 1]
    where
        moveAt pos = Board (board // [(queen, pos)])

-- | counts the number of conflicting queens on the board
conflicts :: Board -> Int
conflicts board = countTrues $ map (uncurry conflict) $ queenPairs board

-- | all possible queen pairs on the board
queenPairs :: Board -> [(Point, Point)]
queenPairs (Board board) = map points $ pairsUnder $ Vec.length board
    where
        points (x1, x2) = ((point x1), (point x2))
        point x = Point x (board ! x)

-- | all pairs (i, j) for 0 <= i < j < n
pairsUnder :: Int -> [(Int, Int)]
pairsUnder n = [(x, y) | (x:ys) <- tails [0..n - 1], y <- ys]

-- | random board with side N
initialBoard :: Int -> IO Board
initialBoard n = makeBoard <$> (replicateM n $ randomRIO (0, n - 1))

-- | check if two queens at the given points are at a conflict
conflict :: Point -> Point -> Bool
conflict a b = (yconflict a b) || (diagconflict a b)
-- no chance of x conflict because we have 1 queen per row

-- | check if two queens are aligned vertically
yconflict :: Point -> Point -> Bool
yconflict (Point _ y1) (Point _ y2) = y1 == y2

-- | check if two queens are aligned diagonally
diagconflict :: Point -> Point -> Bool
diagconflict (Point x1 y1) (Point x2 y2) = (abs $ x1 - x2) == (abs $ y1 - y2)

instance Show Board where
    show (Board board) = "\n" ++ (intercalate "\n" $ map (showQueenAt $ Vec.length board) $ toList board) ++ "\n"

instance Show Point where
    show (Point x y) = "[" ++ (show x) ++ ", " ++ (show y) ++ "]"

-- | board side size
size :: Board -> Int
size (Board queens) = Vec.length queens

-- | make a board from a list of queen positions on their rows
makeBoard :: [Int] -> Board
makeBoard queens = Board $ Vec.fromList queens

showQueenAt :: Int -> Int -> String
showQueenAt size pos = (dup pos "  ") ++ "<>" ++ (dup (size - pos - 1) "  ")

dup :: Int -> String -> String
dup times s = concat $ replicate times s

countTrues :: [Bool] -> Int
countTrues l = length $ filter (== True) l

-- | Returns a random minimal element in the given foldable structure
-- | (by minimal we mean with a minimum f value)
randMinBy :: (Foldable t, Ord b) => (a -> b) -> t a -> IO a
randMinBy f l = sample $ allMinsBy f l

-- | Returns a list of all minimal elements in the given foldable structure
-- | (by minimal we mean with a minimum f value)
allMinsBy :: (Foldable t, Ord b) => (a -> b) -> t a -> [a]
allMinsBy f = foldr lmin []
    where
        lmin x mins
            | null mins                 = [x]
            | f x < (f $ head mins)     = [x]       -- | todo: optimise call to f
            | f x == (f $ head mins)    = x : mins
            | otherwise                 = mins

-- | gets a random element from a list
sample :: [a] -> IO a
sample l = (l !!) <$> (randomRIO (0, (length l) - 1))
