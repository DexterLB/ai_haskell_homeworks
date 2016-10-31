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

conflicts :: Board -> Int
conflicts board = countTrues $ map (uncurry conflict) $ queenPairs board

queenPairs :: Board -> [(Point, Point)]
queenPairs (Board board) = map points $ pairsUnder $ Vec.length board
    where
        points (x1, x2) = ((point x1), (point x2))
        point x = Point x (board ! x)

pairsUnder :: Int -> [(Int, Int)]
pairsUnder n = [(x, y) | (x:ys) <- tails [0..n - 1], y <- ys]

initialBoard :: Int -> IO Board
initialBoard n = makeBoard <$> (replicateM n $ randomRIO (0, n - 1))

conflict :: Point -> Point -> Bool
conflict a b = (xyconflict a b) || (diagconflict a b)

xyconflict :: Point -> Point -> Bool
xyconflict (Point x1 y1) (Point x2 y2) = x1 == x2 || y1 == y2

diagconflict :: Point -> Point -> Bool
diagconflict (Point x1 y1) (Point x2 y2) = (abs $ x1 - x2) == (abs $ y1 - y2)

instance Show Board where
    show (Board board) = "\n" ++ (intercalate "\n" $ map (showQueenAt $ Vec.length board) $ toList board) ++ "\n"

instance Show Point where
    show (Point x y) = "[" ++ (show x) ++ ", " ++ (show y) ++ "]"

size :: Board -> Int
size (Board queens) = Vec.length queens

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
