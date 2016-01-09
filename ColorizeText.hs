module ColorizeText (processInputFile, InputType) where

import qualified Data.Set           as Set
import           ColorizeCommon     (copyTillWord, isWordInCategory,
                                     loadSetFromRange, popWord, skipWord,
                                     tokenize)
import           Data.Char          (toUpper)
import           HtmlDecorator      (generateStatisticsHeader, generateTable,
                                     generateWordListSection, linify,
                                     wrapInHtmlBody, wrapInHtmlSpan,
                                     TableData (TableRow))
import           System.Directory   (getDirectoryContents)
import           System.Posix.Files (getFileStatus, isRegularFile)
import           Text.Printf        (printf)

data InputType = Text | Html deriving (Show, Read)

toBeKnownAnchor :: String
toBeKnownAnchor = "to-be-known"

ignoredAnchor :: String
ignoredAnchor = "ignored"

notFoundAnchor :: String
notFoundAnchor = "not-found"

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
        let notFoundWordsInInput = Set.difference inputSet (Set.unions categories)

        let inputSize = length inputSet
        let knownSize = length knownWordsInInput
        let toBeKnownSize = length toBeKnownWordsInInput
        let ignoredSize = length ignoredWordsInInput
        let notFoundSize = length notFoundWordsInInput

        let summaryHeader = generateStatisticsHeader inputFile knownBoundary targetBoundary
        let headerData = [
                TableRow "Number of input words" inputSize inputSize "normal" "",
                TableRow "Number of known words" knownSize inputSize "normal" "",
                TableRow "Number of to-be-known words" toBeKnownSize inputSize toBeKnownAnchor toBeKnownAnchor,
                TableRow "Number of ignored words" ignoredSize inputSize ignoredAnchor ignoredAnchor,
                TableRow "Number of not-found words" notFoundSize inputSize notFoundAnchor notFoundAnchor
                ]
        let statisticsHeader = generateTable headerData

        putStrLn "Colorizing input based on given boundaries..."

        let colorizedLines = map (linify . process categories) (lines inputText)
        let outputFilename = "index.html"
        writeFile outputFilename (wrapInHtmlBody
                (summaryHeader
                ++ statisticsHeader
                ++ unlines colorizedLines
                ++ generateWordListSection "To Be Known Words" toBeKnownAnchor toBeKnownWordsInInput
                ++ generateWordListSection "Ignored Words" ignoredAnchor ignoredWordsInInput
                ++ generateWordListSection "Not Found Words" notFoundAnchor notFoundWordsInInput
                ))
        putStrLn (printf "Done. Output written to %s" outputFilename)

process :: [Set.Set String] -> String -> String
process _ [] = []
process categories xs =
        colorize headWord categories ++
        charsTillNextWord ++
        process categories tailText
        where
                headWord = popWord xs
                charsTillNextWord = copyTillWord $ skipWord xs
                tailText = drop (length headWord + length charsTillNextWord) xs

colorize :: String -> [Set.Set String] -> String
colorize [] _ = []
colorize xs categories
        | isWordInCategory upperWord (head categories) = xs
        | isWordInCategory upperWord (categories !! 1) = wrapInHtmlSpan xs toBeKnownAnchor
        | isWordInCategory upperWord (categories !! 2) = wrapInHtmlSpan xs ignoredAnchor
        | otherwise = wrapInHtmlSpan xs notFoundAnchor
        where
                upperWord = map toUpper xs
