module ColorizeHtml (processInputFile) where

import           ColorizeCommon     (isWordInCategory, loadSetFromRange,
                                     tokenize)
import           Data.Char          (isAlpha, toLower, toUpper)
import           Data.List          (intercalate, isPrefixOf, sort)
import qualified Data.Set           as Set
import           System.Directory   (getDirectoryContents)
import           System.Posix.Files (getFileStatus, isRegularFile)
import           Text.Printf        (printf)


processInputFile :: FilePath -> Int -> Int -> IO ()
processInputFile inputFile knownBoundary targetBoundary = do
        putStrLn "Loading database and input files..."

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
        let htmlOutputFilename = "index.html"
        writeFile htmlOutputFilename colorizedText
        putStrLn (printf "Html output written to %s" htmlOutputFilename)

        let studyList = collectStudyList toBeKnownWordsInInput False [] inputText
        let studyListFilename = "greenwords.txt"
        writeFile studyListFilename (intercalate "\n" (sort studyList))
        putStrLn (printf "Green words written to %s" studyListFilename)

        let omitList = collectStudyList ignoredWordsInInput False [] inputText
        let omitListFilename = "redwords.txt"
        writeFile omitListFilename (intercalate "\n" (sort omitList))
        putStrLn (printf "Red words written to %s" omitListFilename)



collectStudyList :: Set.Set String -> Bool -> [String] -> String -> [String]
collectStudyList _ _ list [] = list
collectStudyList greenWords inBody results text@(x:xs)
        | isAlpha x = if inBody && Set.member (map toUpper aWord) greenWords
                then collectStudyList greenWords inBody (registerWord aWord results) (dropWhile isAlpha text)
                else collectStudyList greenWords inBody results (dropWhile isAlpha text)
        | x == '<' = collectStudyList greenWords updatedInBody results (dropWhile (/= '>') xs)
        | x == '&' = collectStudyList greenWords inBody results (dropWhile (/= ';') xs)
        | otherwise = collectStudyList greenWords inBody results xs
        where
                aWord = takeWhile isAlpha text
                updatedInBody = inBody || "body " `isPrefixOf` xs || "body>" `isPrefixOf` xs

registerWord :: String -> [String] -> [String]
registerWord aWord list =
        if lowerWord `elem` list
                then increaseWordCount lowerWord list
                else addOnce lowerWord list
        where
                lowerWord = map toLower aWord

addOnce :: String -> [String] -> [String]
addOnce aWord list = aWord : list

-- isWordInStudyList :: String -> [String] -> Bool
-- isWordInStudyList = elem

increaseWordCount :: String -> [String] -> [String]
increaseWordCount _ list = list

process :: [Set.Set String] -> String -> String
process = processRecurse False

processRecurse :: Bool -> [Set.Set String] -> String -> String
processRecurse _ _ [] = []
processRecurse doChange categories text@(x:xs)
        | isAlpha x = colorize doChange aWord categories ++ processRecurse doChange categories (dropWhile isAlpha text)
        | x == '<' = x : takeWhile (/= '>') xs ++ processRecurse updatedDoChange categories (dropWhile (/= '>') xs)
        | x == '&' = x : takeWhile (/= ';') xs ++ processRecurse updatedDoChange categories (dropWhile (/= ';') xs)
        | otherwise = x : processRecurse doChange categories xs
        where
                aWord = takeWhile isAlpha text
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
