import Data.Vector (Vector, toList, (!), (//), MVector)
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as VecM
import Data.List (intercalate, tails, sortBy, groupBy)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Control.Monad.ST (ST)
import Data.Ord (comparing)

-- | the position of each queen on her row
data Board = Board (Vector Int)

main :: IO()
main = do
    n <- readLn
    putStrLn =<< (show <$> solve n)

-- | solve the problem for n queens
solve :: Int -> IO Board
solve n = optimise =<< initialBoard n

-- | shuffle a board so that no queens are in conflict
optimise :: Board -> IO Board
optimise board
    | conflicts board == 0  = pure board
    | otherwise             = optimise =<< optimiseRandomQueen board

-- | move a random queen to a position on her row ehere she causes a minimal
-- | number of conflicts
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

-- | board side size
size :: Board -> Int
size (Board queens) = Vec.length queens

-- | make a board from a list of queen positions on their rows
makeBoard :: [Int] -> Board
makeBoard queens = Board $ Vec.fromList queens

-- | Returns a random minimal element in the given list
-- | (by minimal we mean with a minimum f value)
randMinBy :: (Ord b) => (a -> b) -> [a] -> IO a
randMinBy f = sample . allMinsBy f

-- | Returns a list of all minimal elements in the given list
-- | (by minimal we mean with a minimum f value)
allMinsBy :: (Ord b) => (a -> b) -> [a] -> [a]
allMinsBy f = map snd . head . groupBy (\x y -> fst x == fst y) . sortBy (comparing fst) . map (\x -> (f x, x))

-- | gets a random element from a list
sample :: [a] -> IO a
sample l = (l !!) <$> randomRIO (0, length l - 1)

-- | count the occurances of each number under n in v
-- | ((histogram n v) !! i) == # of occurances of i in v
histogram :: Int -> Vector Int -> Vector Int
histogram n v = Vec.modify (histogram' v) (Vec.replicate n 0)
    where
        histogram' :: Vector Int -> MVector s Int -> ST s ()
        histogram' v hist = mapM_ (\i -> VecM.unsafeModify hist (+ 1) i) v

instance Show Board where
    show (Board board) = intercalate "\n" (map (showQueenAt $ Vec.length board) $ toList board) ++ "\n"

-- | print a single row of the board with a queen at the given position
showQueenAt :: Int -> Int -> String
showQueenAt size pos = dup pos "_" ++ "*" ++ dup (size - pos - 1) "_"

-- | repeat a string several times
dup :: Int -> String -> String
dup times = concat . replicate times
