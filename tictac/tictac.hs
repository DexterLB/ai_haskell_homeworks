import Data.Matrix (Matrix, prettyMatrix, (!))
import qualified Data.Matrix as Matrix
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Graphics.QML (defaultEngineConfig, runEngineLoop, initialDocument, contextObject)
import Graphics.QML (fileDocument, anyObjRef, newClass, defMethod', newObject)

data Board = Board Int (Matrix Player)

data Player = O | X | None deriving (Eq)

main :: IO ()
main = do
    clazz <- newClass [
        defMethod' "solve" (\_ player board -> 
            return (textSolve player board) :: IO Text)]

    ctx <- newObject clazz ()

    runEngineLoop defaultEngineConfig {
        initialDocument = fileDocument "tictac_gui.qml",
        contextObject = Just $ anyObjRef ctx}

textSolve :: Text -> Text -> Text
textSolve playerT boardT = Text.pack $ stringSolve playerS boardS
    where
        playerS = Text.unpack playerT
        boardS = Text.unpack boardT

stringSolve :: String -> String -> String
stringSolve playerS boardS = stringifyBoard result
    where
        result = solve player board
        player = readPlayer (head playerS)
        board  = readBoard boardS

solve :: Player -> Board -> Board
solve player board = board'
    where
        (_, board') = αβ player (α, β) board
        α = -inf
        β = inf

αβ :: Player -> (Int, Int) -> Board -> (Int, Board)
αβ player (α, β) board
    | null children = (leafValue $ winner board, board)
    | otherwise     = best
    where
        (_, _, best) = foldr (iterateαβ player) bottom children
        bottom = (α, β, (bottomFor player, board))
        children = childrenFor player board

iterateαβ :: Player -> Board -> (Int, Int, (Int, Board)) -> (Int, Int, (Int, Board))
iterateαβ player child (α,  β,  best)
    | β <= α        =  (α,  β,  best)
    | player == X   =  (α', β,  best')
    | player == O   =  (α,  β', best')
    where
        α' = max α bestValue
        β' = min β bestValue
        bestValue = fst best'
        best' = minBy (<|>) best (childValue, child)
        (childValue, _) = αβ (other player) (α, β) child
        (<|>) (x, _) (y, _) = compFor player x y


winner :: Board -> Player
winner = foldr1 (<>) . map rowWinner . possibleWins

childrenFor :: Player -> Board -> [Board]
childrenFor player (Board size m) = map setAt $ emptySpaces (Board size m)
    where
        setAt pos = Board size (Matrix.setElem player pos m)

emptySpaces :: Board -> [(Int, Int)]
emptySpaces (Board size m) = filter (\pos -> m ! pos == None) positions
    where
        positions = [(x, y) | x <- [1..size], y <- [1..size]]

rowWinner :: Vector Player -> Player
rowWinner r
    | all (== first) r  = first
    | otherwise         = None
    where
        first = r Vector.! 0

possibleWins :: Board -> [Vector Player]
possibleWins b = rows b ++ cols b ++ [mainDiag b, oppositeDiag b]

rows :: Board -> [Vector Player]
rows (Board size m) = map (`Matrix.getRow` m) [1..size]

cols :: Board -> [Vector Player]
cols (Board size m) = map (`Matrix.getCol` m) [1..size]

mainDiag :: Board -> Vector Player
mainDiag (Board size m) = Vector.generate size elem
    where
        elem i = m ! (i + 1, i + 1)

oppositeDiag :: Board -> Vector Player
oppositeDiag (Board size m) = Vector.generate size elem
    where
        elem i = m ! (i + 1, size - i)

readBoard :: String -> Board
readBoard s = Board size $ Matrix.fromList size size $ map readPlayer s
    where
        size :: Int
        size = truncate $ sqrt $ fromIntegral $ length s

stringifyBoard :: Board -> String
stringifyBoard (Board _ m) = foldr1 (++) $ map show $ Matrix.toList m

readPlayer :: Char -> Player
readPlayer 'X' = X
readPlayer 'O' = O
readPlayer _   = None

instance Show Player where
    show O      = "O"
    show X      = "X"
    show None   = " "

instance Show Board where
    show (Board _ matrix) = "\n" ++ prettyMatrix matrix ++ "\n"

other :: Player -> Player
other X    = O
other O    = X
other None = None

leafValue :: Player -> Int
leafValue X     = 1
leafValue O     = -1
leafValue None  = 0

bottomFor :: Player -> Int
bottomFor X     = -inf
bottomFor O     = inf
bottomFor None  = 0

compFor :: (Ord a) => Player -> (a -> a -> Bool)
compFor X       = (>)
compFor O       = (<)
compFor None    = (==)

instance Monoid Player where
    mempty = None

    mappend None x = x
    mappend x None = x
    mappend x _ = x


topBy :: Foldable t => (a -> a -> Bool) -> t a -> a
topBy f = foldr1 (minBy f)

minBy :: (a -> a -> Bool) -> a -> a -> a
minBy f x y
    | x `f` y   = x
    | otherwise = y

inf :: Int
inf = 999999
