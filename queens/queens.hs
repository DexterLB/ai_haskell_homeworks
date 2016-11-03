import Data.Vector (Vector, toList, (!), (//))
import Data.List (intercalate, tails, sortBy, groupBy)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.Ord (comparing)
import Control.Arrow
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as VecM

data Point = Point Int Int

data Board = Board (Vector Int)


main :: IO()
main = do
    n <- readLn
    putStrLn =<< (show <$> solve n)

solve :: Int -> IO Board
solve n = optimise =<< initialBoard n

optimise :: Board -> IO Board
optimise board
    | conflicts board == 0  = pure board
    | otherwise             = optimise =<< optimiseRandomQueen board

optimiseRandomQueen :: Board -> IO Board
optimiseRandomQueen board = optimiseQueen board =<< randomRIO (0, size board - 1)

-- | move the queen with the given index to a position where she causes a minimal
-- | number of conflicts
optimiseQueen :: Board -> Int -> IO Board
optimiseQueen (Board board) queen = randMinBy conflicts $ map moveAt [0..length board - 1]
    where
        moveAt pos = Board (board // [(queen, pos)])

-- | counts the number of conflicting queens on the board
conflicts :: Board -> Int
conflicts board = conflictsFor (rowIndices board)
                + conflictsFor (mainDiagIndices board)
                + conflictsFor (secDiagIndices board)

-- | get the number of conflicts for a list of positions
-- | (e.g. row indices, diagonal indices etc)
conflictsFor :: (Vector Int, Int) -> Int
conflictsFor (indices, maxIndex) = Vec.sum $ Vec.map alignedConflicts hist
    where hist = histogram maxIndex indices

-- | the number of conflicts between n aligned queens
alignedConflicts :: Int -> Int
alignedConflicts n = (n * (n - 1)) `div` 2

rowIndices :: Board -> (Vector Int, Int)
rowIndices (Board board) = (board, Vec.length board)

mainDiagIndices :: Board -> (Vector Int, Int)
mainDiagIndices (Board board) = (indices, numDiags + 1)
    where
        indices = Vec.zipWith diagIndex board $ Vec.fromList [0..boardSide - 1]
        diagIndex x y = x - y  + (numDiags `div` 2)
        numDiags = (boardSide - 1) * 2
        boardSide = Vec.length board

secDiagIndices :: Board -> (Vector Int, Int)
secDiagIndices (Board board) = (indices, numDiags + 1)
    where
        indices = Vec.zipWith diagIndex board $ Vec.fromList [0..boardSide - 1]
        diagIndex x y = x + y
        numDiags = (boardSide - 1) * 2
        boardSide = Vec.length board

-- | random board with side N
initialBoard :: Int -> IO Board
initialBoard n = makeBoard <$> replicateM n (randomRIO (0, n - 1))

instance Show Board where
    show (Board board) = "\n" ++ intercalate "\n" (map (showQueenAt $ Vec.length board) $ toList board) ++ "\n"

instance Show Point where
    show (Point x y) = "[" ++ show x ++ ", " ++ show y ++ "]"

-- | board side size
size :: Board -> Int
size (Board queens) = Vec.length queens

-- | make a board from a list of queen positions on their rows
makeBoard :: [Int] -> Board
makeBoard queens = Board $ Vec.fromList queens

showQueenAt :: Int -> Int -> String
showQueenAt size pos = dup pos "  " ++ "<>" ++ dup (size - pos - 1) "  "

dup :: Int -> String -> String
dup times = concat . replicate times

countTrues :: [Bool] -> Int
countTrues = length . filter (== True)

-- | Returns a random minimal element in the given foldable structure
-- | (by minimal we mean with a minimum f value)
randMinBy :: (Ord b) => (a -> b) -> [a] -> IO a
randMinBy f = sample . allMinsBy f

-- | Returns a list of all minimal elements in the given foldable structure
-- | (by minimal we mean with a minimum f value)
allMinsBy :: (Ord b) => (a -> b) -> [a] -> [a]
allMinsBy f = map snd . head . groupBy (\x y -> fst x == fst y) . sortBy (comparing fst) . map (\x -> (f x, x))

-- | gets a random element from a list
sample :: [a] -> IO a
sample l = (l !!) <$> randomRIO (0, length l - 1)

-- | count the occurances of each number under n in v
-- | ((histogram n v) !! i) == # of occurances of i in v
histogram :: Int -> Vector Int -> Vector Int
histogram n = foldr incrementPos (Vec.replicate n 0)

-- | incrementPos increases the value at position i by one
incrementPos :: Int -> Vector Int  -> Vector Int
incrementPos i = Vec.modify (\v -> VecM.unsafeModify v (+ 1) i)
