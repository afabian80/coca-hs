module ColorizeCommon
(
tokenize
,popWord
,skipWord
,copyTillWord
,isWordInCategory
,loadSetFromRange
) where

import qualified Data.Set        as Set
import           Data.Char       (isAlpha, toUpper)
import           Data.List.Split (splitWhen)
import           Text.Printf     (printf)

tokenize :: String -> Set.Set String
tokenize text = Set.fromList uppercaseWords
        where
                uppercaseWords = splitWhen (not . isAlpha) (map toUpper text)

isWordInCategory :: String -> Set.Set String -> Bool
isWordInCategory = Set.member

popWord :: String -> String
popWord = takeWhile isAlpha

skipWord :: String -> String
skipWord = dropWhile isAlpha

copyTillWord :: String -> String
copyTillWord = takeWhile (not . isAlpha)

loadSetFromRange :: String -> Int -> Int -> IO (Set.Set String)
loadSetFromRange dbDir lo hi = do
        let fileNames = generateFileNames dbDir lo hi
        sets <- mapM loadSet fileNames
        return (Set.unions sets)

generateFileNames :: String -> Int -> Int -> [String]
generateFileNames dbDir lo hi =
        map (printf (dbDir ++ "/basewrd%02d.txt")) [lo..hi]

loadSet :: String -> IO (Set.Set String)
loadSet path = do
        fileText <- readFile path
        let wordSet = Set.fromDistinctAscList (words fileText)
        return wordSet
