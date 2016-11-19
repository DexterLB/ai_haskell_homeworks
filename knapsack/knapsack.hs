import Data.List (sortOn)
import Data.List.Split (splitOneOf)
import Data.Word8 (Word8)
import Control.Monad (replicateM)
import System.Random (randomRIO, randomIO)

import Debug.Trace (trace)

type TrutsyFalsey = [Word8]

data Knapsack = Knapsack {
    prices      :: [Int]
  , weights     :: [Int]
  , size        :: Int
}

main = do
    input <- getContents
    result <- io input
    putStrLn result

io :: String -> IO String
io input = (showSolution knapsack) <$> (solve knapsack (2, 100) maxWeight 200 200)
    where
        (knapsack, maxWeight) = readKnapsack input


solve :: Knapsack           -- ^ knapsack description
      -> (Int, Int)         -- ^ mutation probablity
      -> Int                -- ^ maximum weight
      -> Int                -- ^ iterations
      -> Int                -- ^ population size
      -> IO TrutsyFalsey
solve k mutationProbability maxWeight iterations populationSize = solution
    where
        solution = (genetic k mutationProbability maxWeight iterations) =<< population
        population = randomPopulation k populationSize

genetic :: Knapsack         -- ^ knapsack description
        -> (Int, Int)       -- ^ mutation probability
        -> Int              -- ^ maximum weight
        -> Int              -- ^ iterations
        -> [TrutsyFalsey]   -- ^ starting population
        -> IO TrutsyFalsey
genetic k mutationProbability maxWeight iterations population
    | iterations <= 0 = (headByPrice k) <$> clamped
    | otherwise       = (genetic k mutationProbability maxWeight (iterations - 1)) =<< population'
    where
        population' :: IO [TrutsyFalsey]
        population' = (breed k) =<< (mutateAll mutationProbability) =<< clamped
        clamped = clampAll k maxWeight population

breed :: Knapsack -> [TrutsyFalsey] -> IO [TrutsyFalsey]
breed k p = (foldr (++) []) <$> ((mapM cross) =<< selectPairs k p)

selectPairs :: Knapsack -> [TrutsyFalsey] -> IO [(TrutsyFalsey, TrutsyFalsey)]
selectPairs k population = indexPairs population <$> selectedIndices
    where
        selectedIndices :: IO [(Int, Int)]
        selectedIndices = weighedRandomPairs populationPrices numPairs

        numPairs = (size k) `div` 2

        populationPrices :: [Int]
        populationPrices = map (priceValue . (price k)) population

indexPairs :: [a] -> [(Int, Int)] -> [(a, a)]
indexPairs source = map (\(i, j) -> (source !! i, source !! j))

mutateAll :: (Int, Int) -> [TrutsyFalsey] -> IO [TrutsyFalsey]
mutateAll probability = mapM (mutate probability)

mutate :: (Int, Int) -> TrutsyFalsey -> IO TrutsyFalsey
mutate (num, denom) tf = mutate' =<< randomRIO (0, denom)
    where
        mutate' val
            | val <= num = flipRandom tf
            | otherwise  = pure tf

cross :: (TrutsyFalsey, TrutsyFalsey) -> IO [TrutsyFalsey]
cross (x, y) = (\n -> crossAt n (x, y)) <$> (randomRIO (0, length x))

crossAt :: Int -> (TrutsyFalsey, TrutsyFalsey) -> [TrutsyFalsey]
crossAt n (x, y) = [leftright, rightleft]
    where
        leftright = leftx ++ righty
        rightleft = lefty ++ rightx
        (leftx, rightx) = splitAt n x
        (lefty, righty) = splitAt n y

clampAll :: Knapsack -> Int -> [TrutsyFalsey] -> IO [TrutsyFalsey]
clampAll k maxWeight = mapM (clamp k maxWeight)

clamp :: Knapsack -> Int -> TrutsyFalsey -> IO TrutsyFalsey
clamp k maxWeight tf
    | weight k tf <= maxWeight = pure tf
    | otherwise = (clamp k maxWeight) =<< (flipRandomOne tf)

flipRandomOne :: TrutsyFalsey -> IO TrutsyFalsey
flipRandomOne tf = (\pos -> update pos 0 tf) <$> (randomOne tf)

flipRandom :: TrutsyFalsey -> IO TrutsyFalsey
flipRandom tf = (\pos -> update pos (1 - (tf !! pos)) tf) <$> (randomRIO (0, length tf - 1))

randomOne :: TrutsyFalsey -> IO Int
randomOne tf = (\i -> ones !! i) <$> (randomRIO (0, length ones - 1))
    where
        ones = onePositions tf

onePositions :: (Num a, Eq a) => [a] -> [Int]
onePositions l = filter (\i -> l !! i == 1) [0..length l - 1]

newKnapsack :: [Int] -> [Int] -> Knapsack
newKnapsack p w = Knapsack { prices = p, weights = w, size = length p }

randomPopulation :: Knapsack -> Int -> IO [TrutsyFalsey]
randomPopulation Knapsack { size = s } m = replicateM m (randomTrutsyFalsey s)

randomTrutsyFalsey :: Int -> IO TrutsyFalsey
randomTrutsyFalsey n = replicateM n $ randomRIO ((0, 1) :: (Word8, Word8))

headByPrice :: Knapsack -> [TrutsyFalsey] -> TrutsyFalsey
headByPrice k = head . (sortOn (price k))

price :: Knapsack -> TrutsyFalsey -> Int
price Knapsack { prices = p } = dotProduct p

weight :: Knapsack -> TrutsyFalsey -> Int
weight Knapsack { weights = w } = dotProduct w

dotProduct :: [Int] -> TrutsyFalsey -> Int
dotProduct x y = sum $ (zipWith (\x y -> x * (fromIntegral y)) x y)

weighedRandomPairs :: [Int] -> Int -> IO [(Int, Int)]
weighedRandomPairs weights n = pairify <$> sample
    where
        pairify :: [Int] -> [(Int, Int)]
        pairify l = (uncurry zip) $ splitAt n l
        sample = weighedRandom weights (n * 2)

weighedRandom :: [Int] -> Int -> IO [Int]
weighedRandom weights n = randomSample aweights max n
    where
        max = fst $ last aweights
        aweights = cumulativeWeights weights

randomSample :: [(Int, Int)] -> Int -> Int -> IO [Int]
randomSample aweights max n = replicateM n (randomIndex aweights max)

randomIndex :: [(Int, Int)] -> Int -> IO Int
randomIndex aweights max = (weighedIndex aweights) <$> (randomRIO (0, max))

weighedIndex :: [(Int, Int)] -> Int -> Int
weighedIndex aweights i = snd $ aweights !! index
    where
        index = weighedIndex' aweights i 0 (length aweights - 1)

        weighedIndex' :: [(Int, Int)] -> Int -> Int -> Int -> Int
        weighedIndex' aweights i a b
            | a > b                 = a
            | midElement < i        = weighedIndex' aweights i (mid + 1) b
            | midElement > i        = weighedIndex' aweights i a (mid - 1)
            | otherwise             = mid
            where
                mid = (a + b) `div` 2
                midElement = fst $ aweights !! mid

cumulativeWeights :: [Int] -> [(Int, Int)]
cumulativeWeights = accumulate . sortedWeights

sortedWeights :: [Int] -> [(Int, Int)]
sortedWeights = (sortOn fst) . (`zip` [0..])

accumulate :: [(Int, Int)] -> [(Int, Int)]
accumulate = scanl1 add
    where
        add (w1, i1) (w2, i2) = (w1 + w2, i2)

update :: Int -> a -> [a] -> [a]
update i x l = (take i l) ++ [x] ++ (drop (i + 1) l)

showSolution :: Knapsack -> TrutsyFalsey -> String
showSolution k tf = (show $ price k tf) ++ "\n" ++ (show tf)

readKnapsack :: String -> (Knapsack, Int)
readKnapsack s = (newKnapsack prices weights, maxWeight)
    where
        (prices, weights) = unzip $ pairList $ tail $ tail ints
        maxWeight = head ints

        ints :: [Int]
        ints = map read $ filter (not . null) $ splitOneOf "\n\r " s

pairList :: [a] -> [(a, a)]
pairList [] = []
pairList (k:v:t) = (k, v) : pairList t

priceValue :: Int -> Int
priceValue i = i * i
