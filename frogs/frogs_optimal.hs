{-# LANGUAGE OverloadedStrings #-}

import Data.Vector (Vector, fromList, toList, (!), (//))
import Data.Maybe (Maybe, mapMaybe)
import Control.Monad (msum)

type Node = (
                Int,        -- position of empty slot
                Vector Char -- array of frogs ('<' facing left and '>' facing right)
            )

main :: IO()
main = do
    frogs <- (readLn :: IO Int)
    printSolution frogs

printSolution :: Int -> IO()
printSolution frogs = printLines $ fmap (nodesToStrings . reverse) $ solve frogs

solve :: Int -> Maybe [Node]
solve frogs = fmap reverse $ dfs ((frogs + 1) ^ 2 - 1) $ initialNode frogs

dfs :: Int -> Node -> Maybe [Node]
dfs depth node

    | depth == 0                    = Just [node]
    | otherwise                     = fmap (node :) (msum child_paths)

    where child_paths = map (dfs (depth - 1)) $ children node


children :: Node -> [Node]
children (e, s) = walkLeft ++ walkRight ++ jumpLeft ++ jumpRight
    where
        walkLeft
            | (e < (length s - 1)) && (s ! (e + 1) == '<') = [(e + 1, s // [(e, '<')])]
            | otherwise                                    = []
        walkRight
            | (e > 0) && (s ! (e - 1) == '>')              = [(e - 1, s // [(e, '>')])]
            | otherwise                                    = []
        jumpLeft
            | (e < (length s - 2)) && (s ! (e + 2) == '<') = [(e + 2, s // [(e, '<')])]
            | otherwise                                    = []
        jumpRight
            | (e > 1) && (s ! (e - 2) == '>')              = [(e - 2, s // [(e, '>')])]
            | otherwise                                    = []


initialNode :: Int -> Node
initialNode frogs = (frogs, (fromList start))
                    where start = (replicate frogs '>') ++
                                  "_"                   ++
                                  (replicate frogs '<')

nodesToStrings :: [Node] -> [String]
nodesToStrings nodes = map (toList . realSlots) nodes

realSlots :: Node -> Vector Char
realSlots (e, s) = s // [(e, '_')]

printLines :: Maybe [String] -> IO()
printLines (Just lines) = mapM_ putStrLn lines
printLines Nothing      = putStrLn "no solution"
