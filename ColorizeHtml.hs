module ColorizeHtml (processInputFile, InputType) where

import qualified Data.Set           as Set
import           ColorizeCommon     (isWordInCategory,
                                     loadSetFromRange,
                                     tokenize)
import           Data.Char          (toUpper, isAlpha)
import           System.Directory   (getDirectoryContents)
import           System.Posix.Files (getFileStatus, isRegularFile)
import           Text.Printf        (printf)
import Data.List (isPrefixOf)

data InputType = Text | Html deriving (Show, Read)


processInputFile :: FilePath -> InputType -> Int -> Int -> IO ()
processInputFile inputFile inputType knownBoundary targetBoundary = do
        putStrLn "Loading database and input files..."
        print inputType

        let dbDir = "cocadb"
        dbFileNames <- getDirectoryContents dbDir
        let dbRelativeNames = map ((dbDir ++ "/") ++) dbFileNames
        dbFileStatuses <- mapM getFileStatus dbRelativeNames
        let fileStatusPairs = zip dbFileNames (map isRegularFile dbFileStatuses)
        let regularDbFiles = [f | (f,st) <- fileStatusPairs, st]

        inputText <- readFile inputFile
        let inputSet = tokenize inputText

        knownWordSet <- loadSetFromRange dbDir 1 knownBoundary
        toBeKnownWordSet <- loadSetFromRange dbDir (knownBoundary + 1) targetBoundary
        ignoredWordSet <- loadSetFromRange dbDir (targetBoundary + 1) (length regularDbFiles)

        let knownWordsInInput = Set.intersection inputSet knownWordSet
        let toBeKnownWordsInInput = Set.intersection inputSet toBeKnownWordSet
        let ignoredWordsInInput = Set.intersection inputSet ignoredWordSet

        let categories = [knownWordsInInput, toBeKnownWordsInInput, ignoredWordsInInput]

        putStrLn "Colorizing input based on given boundaries..."

        let colorizedText = process categories inputText
        let outputFilename = "index.html"
        writeFile outputFilename colorizedText
        putStrLn (printf "Done. Output written to %s" outputFilename)

process :: [Set.Set String] -> String -> String
process = processRecurse False

processRecurse :: Bool -> [Set.Set String] -> String -> String
processRecurse _ _ [] = []
processRecurse doChange categories text@(x:xs)
        | isAlpha x = colorize doChange (takeWhile isAlpha text) categories ++ processRecurse doChange categories (dropWhile isAlpha text)
        | x == '<' = x : takeWhile (/= '>') xs ++ processRecurse updatedDoChange categories (dropWhile (/= '>') xs)
        | x == '&' = x : takeWhile (/= ';') xs ++ processRecurse updatedDoChange categories (dropWhile (/= ';') xs)
        | otherwise = x : processRecurse doChange categories xs
        where
                updatedDoChange = doChange || "body " `isPrefixOf` xs || "body>" `isPrefixOf` xs

colorize :: Bool -> String -> [Set.Set String] -> String
colorize _ [] _ = []
colorize doColorize xs categories
        | isWordInCategory upperWord (head categories) = xs
        | isWordInCategory upperWord (categories !! 1) =
                if doColorize
                        then "<span style=\"background-color:LightGreen;\">" ++ xs ++ "</span>"
                        else xs
        | isWordInCategory upperWord (categories !! 2) =
                if doColorize
                        then "<span style=\"background-color:Salmon;\">" ++ xs ++ "</span>"
                        else xs
        | otherwise =
                if doColorize
                        then "<span style=\"background-color:LightGrey;\">" ++ xs ++ "</span>"
                        else xs
        where
                upperWord = map toUpper xs
