module Main where

import Data.CSV (csvFile)
import Data.Either.Unwrap (fromRight)
import Text.ParserCombinators.Parsec (parseFromFile, ParseError)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.List (sortOn, sort, group)
import System.Random.Shuffle (shuffle')
import System.Random (newStdGen)
import Control.Monad (mapM_)
import Control.Arrow ( (&&&) )

data Flower = Flower {
    klass    :: String
  , measures :: Vector Float
}

main :: IO ()
main = do
    allFlowers <- parseFlowers <$> readData "iris.csv"
    (tested, testers) <- randomSplit 20 allFlowers
    let estimates = estimateFlowers 4 testers tested
    let accurate = countAccurate estimates

    mapM_ (putStrLn . showClassCheck) estimates

    putStrLn ("accurate: " ++ show accurate ++ "/" ++ show (length estimates))

-- | estimates the alleged class of a group of flowers
estimateFlowers :: Int -> [Flower] -> [Flower] -> [(String, Flower)]
estimateFlowers k testers = map (\t -> (knearest k testers (measures t), t))

-- | estimates the specimen class
knearest :: Int -> [Flower] -> Vector Float -> String
knearest k flowers specimen = mostCommon $ take k $ map klass $ nearest flowers specimen

-- | enumerates the flower set in ascending order by distance from the specimen
nearest :: [Flower] -> Vector Float -> [Flower]
nearest flowers specimen = sortOn (`distToFlower` specimen) flowers

-- | computes the distance of a specimen to a flower
distToFlower :: Flower -> Vector Float -> Float
distToFlower flower = dist (measures flower)

-- | distance between two specimens (euclidian)
dist :: Vector Float -> Vector Float -> Float
dist a b = Vector.sum $ Vector.map (** 2) $ Vector.zipWith (-) a b

-- | returns the most common element in a list
mostCommon :: (Ord a) => [a] -> a
mostCommon = snd . maximum . map mark . group . sort
   where
      mark (a:as) = (1 + length as, a)

-- | splits a list into two bins, one with n elements and the other with the rest
randomSplit :: Int -> [a] -> IO ([a], [a])
randomSplit n flowers = (take n &&& drop n) <$> perm'
    where
        perm' = shuffle flowers

-- | reads data from CSV
readData :: String -> IO [[String]]
readData = fmap fromRight . parseFromFile csvFile

-- | parses CSV values into flowers
parseFlowers :: [[String]] -> [Flower]
parseFlowers = map parseFlower

-- | parses single CSV row into a flower
parseFlower :: [String] -> Flower
parseFlower items = Flower {
    klass = last items
  , measures = Vector.fromList $ map read $ init items
}

-- | shuffles a list
shuffle :: [a] -> IO [a]
shuffle l = shuffle' l (length l) <$> newStdGen

-- | shows a human-readable summary on an estimate and whether it was accurate
showClassCheck :: (String, Flower) -> String
showClassCheck (estimatedClass, flower) = show flower ++ " -> " ++ estimatedClass ++ check
    where
        check
            | klass flower == estimatedClass = " [yes]"
            | otherwise                      = " [no] "

-- | count the number of accurate estimates in a list of estimates
countAccurate :: [(String, Flower)] -> Int
countAccurate = sum . map isAccurate
    where
        isAccurate (estimatedClass, flower)
            | klass flower == estimatedClass = 1
            | otherwise                      = 0

instance Show Flower where
    show Flower {klass = k, measures = m} = k ++ show m
