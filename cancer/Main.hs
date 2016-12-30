module Main where

import Data.Function (on)
import Data.CSV (csvFile)
import Data.Either.Unwrap (fromRight)
import Text.ParserCombinators.Parsec (parseFromFile, ParseError)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.List (sortOn, sort, group, intercalate, groupBy, elemIndex, maximumBy)
import Data.Ord (comparing)
import System.Random.Shuffle (shuffle')
import System.Random (newStdGen)
import Control.Monad (mapM_)
import Data.Maybe (fromJust)
import Control.Arrow ( (&&&) )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Debug.Trace (trace)

data Frame = Frame {
    names    :: [String]
  , values   :: [[String]]
}

data Tree = Tree {
    attribute   :: String
  , children    :: HashMap String Tree
  , klass       :: String
} deriving (Show)

main :: IO ()
main = do
    frame <- parseData <$> readData "breast-cancer.csv"
    (tested, testers) <- randomSplitFrame frame 10
    let tree = buildTree testers
    let requests = namedRows tested
    let results = predictMany tree requests

    putStrLn $ intercalate "\n" $ map show results
    putStrLn $ "accuracy: " ++ (accuracy results)


accuracy :: [(HashMap String String, String)] -> String
accuracy results = (show $ length $ accurate results) ++ "/" ++ (show $ length results)
    where
        accurate = filter (\(req, result) -> (req HashMap.! "Class") == result)

predictMany :: Tree -> [HashMap String String] -> [(HashMap String String, String)]
predictMany tree requests = zip requests (map (predict tree) requests)

-- | predict a class using the ID3 tree
predict :: Tree -> HashMap String String -> String
predict tree req
    | not $ HashMap.member (attribute tree) req  = klass tree
    | not $ HashMap.member value (children tree) = klass tree
    | otherwise = predict ((children tree) HashMap.! value) req
    where
        value = req HashMap.! (attribute tree)

-- | builds an ID3 tree on the data
buildTree :: Frame -> Tree
buildTree frame = Tree { attribute = attribute, children = children, klass = klass }
    where
        children = fmap buildTree childFrames
        klass = mostCommonClass frame
        (attribute, childFrames) = bestSplit frame

-- | calculates the information gain of the split data
igain :: Frame -> HashMap String Frame -> Float
igain frame splitFrame = (entropy frame) - (entropyAfterSplit)
    where
        entropyAfterSplit = sum $ map weight $ subsets

        weight :: Frame -> Float
        weight subset = ((fromIntegral $ length $ values subset) / all) * entropy subset

        all = fromIntegral $ length $ values frame
        subsets = HashMap.elems $ splitFrame

-- | calculates the entropy of the entire data frame
entropy :: Frame -> Float
entropy frame = colEntropy $ classColumn frame

-- | calculates the entropy of a collection of classes
colEntropy :: [String] -> Float
colEntropy classes = - (sum $ map (weight . fromIntegral) $ classCounts)
    where
        weight :: Float -> Float
        weight count = (count / all) * (logBase 2 (count / all))

        all = fromIntegral $ length classes
        classCounts = map length $ groupSortOn id classes

-- | splits the data by the column with best information gain
bestSplit :: Frame -> (String, HashMap String Frame)
bestSplit frame
    | null $ allSplits frame = ("Class", HashMap.empty)
    | otherwise = maximumBy (comparing $ (igain frame) . snd) $ allSplits frame

-- | splits the data by all columns (each returned tuple contains the column name and the split)
allSplits :: Frame -> [(String, HashMap String Frame)]
allSplits frame = map (fmap $ split frame) columns
    where
        columns = filter ((/= "Class") . fst) $ zip (names frame) [0..]

-- | split the data, grouping it by the column with index i
split :: Frame                   -- ^ data frame to split
      -> Int                     -- ^ index of column to split by
      -> HashMap String Frame    -- ^ grouping
split (Frame {names = n, values = v}) i = HashMap.fromList children
    where
        children :: [(String, Frame)]
        children = map pruneGroup groups

        pruneGroup :: [[String]] -> (String, Frame)
        pruneGroup group = ((group !! 0 !! i), Frame (pruneRow n) (map pruneRow group))

        pruneRow :: [a] -> [a]
        pruneRow = removeAt i

        groups :: [[[String]]]
        groups = groupSortOn (!! i) v

mostCommonClass :: Frame -> String
mostCommonClass = mostCommon . classColumn

-- | returns the class column of the data frame
classColumn :: Frame -> [String]
classColumn frame = column (forceElemIndex "Class" $ names frame) frame

-- | group an unsorted list by keys from a function
groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = (groupBy ((==) `on` f)) . (sortOn f)

-- | remove the i'th element from list
removeAt :: Int -> [a] -> [a]
removeAt i l = (take i l) ++ (drop (i + 1) l)

-- | reads data from CSV
readData :: String -> IO [[String]]
readData = fmap fromRight . parseFromFile csvFile

-- | parses data
parseData :: [[String]] -> Frame
parseData l = Frame {
        names   = head l
      , values  = tail l
    }


-- | transposes data
columns :: Frame -> [[String]]
columns frame = map (\i -> column i frame) [0..(length $ names frame) - 1]

-- | gets the i'th column of the frame
column :: Int -> Frame -> [String]
column i = map (!! i) . values

-- | returns the frame as a list of maps
namedRows :: Frame -> [HashMap String String]
namedRows (Frame { names = n, values = v }) = map HashMap.fromList rowMaps
    where
        rowMaps :: [[(String, String)]]
        rowMaps = map (uncurry zip) $ zip (cycle [n]) v

randomSplitFrame :: Frame -> Int -> IO (Frame, Frame)
randomSplitFrame (Frame { names = n, values = v }) i = do
    (left, right) <- randomSplit i v
    return (Frame { names = n, values = left }, Frame { names = n, values = right })

instance Show Frame where
    show Frame {names = n, values = v} = "\n***\n" ++ showList n ++ "\nvvv\n" ++ showLists v ++ "\n^^^\n"
        where
            showList = intercalate " "
            showLists = (intercalate "\n") . (map showList)

forceElemIndex :: Eq a => a -> [a] -> Int
forceElemIndex a l = fromJust $ elemIndex a l

-- | returns the most common element in a list
mostCommon :: (Ord a) => [a] -> a
mostCommon = snd . maximum . map mark . group . sort
   where
      mark (a:as) = (1 + length as, a)

-- | splits a list into two bins, one with n elements and the other with the rest
randomSplit :: Int -> [a] -> IO ([a], [a])
randomSplit n items = (take n &&& drop n) <$> perm
    where
        perm = shuffle items

-- | shuffles a list
shuffle :: [a] -> IO [a]
shuffle l = shuffle' l (length l) <$> newStdGen
