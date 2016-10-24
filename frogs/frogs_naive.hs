{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (replicate, replace)
import Data.Text (Text, replicate, replace, unpack, append)
import Data.Maybe (Maybe, mapMaybe)
import Text.Regex.PCRE ((=~))
import Control.Monad (msum)

main :: IO()
main = do
    frogs <- (readLn :: IO Int)
    printSolution frogs

printSolution :: Int -> IO()
printSolution frogs = printLines $ solve frogs

solve :: Int -> Maybe [Text]
solve frogs = fmap reverse $ dfs start
              where start = (replicate frogs ">") `append`
                            "_" `append`
                            (replicate frogs "<")

dfs :: Text -> Maybe [Text]
dfs node

    | final                         = Just [node]
    | otherwise                     = fmap (++ [node]) (msum child_paths)

    where child_paths = map dfs $ children node
          final = isFinal node


children :: Text -> [Text]
children s = removeNothings $
    [replaceNotEqual ">_" "_>" s] ++
    [replaceNotEqual "_<" "<_" s] ++
    [replaceNotEqual ">>_" "_>>" s] ++
    [replaceNotEqual "_<<" "<<_" s] ++
    [replaceNotEqual "><_" "_<>" s] ++
    [replaceNotEqual "_><" "<>_" s]


replaceNotEqual :: Text -> Text -> Text -> Maybe Text
replaceNotEqual needle replacement haystack

    | result == haystack = Nothing
    | otherwise = Just result

    where result = replace needle replacement haystack


removeNothings :: [Maybe a] -> [a]
removeNothings list = mapMaybe id list

isFinal :: Text -> Bool
isFinal node = (unpack node) =~ (unpack "^<*_>*$")

printLines :: Maybe [Text] -> IO()
printLines (Just lines) = mapM_ (putStrLn . unpack) lines
printLines Nothing      = putStrLn "no solution"
