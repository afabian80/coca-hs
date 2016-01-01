import System.Environment (getArgs)
import Data.Char (isAlpha, toUpper, toLower)
import qualified Data.Set as Set
import Text.Printf (printf)
import Data.List.Split (splitWhen)
import System.Directory (getDirectoryContents)
import System.Posix.Files (getFileStatus, isRegularFile)
import Data.List (zip5)

toBeKnownAnchor :: String
toBeKnownAnchor = "to-be-known"

ignoredAnchor :: String
ignoredAnchor = "ignored"

notFoundAnchor :: String
notFoundAnchor = "not-found"

main :: IO ()
main = do
        args <- getArgs
        case args of
                [inputFile, knownBoundary, targetBoundary] ->
                        processInputFile inputFile knownBoundary targetBoundary
                _ -> putStrLn "Use parameters: <inputfile> <known> <to-be-known>"

processInputFile :: FilePath -> String -> String -> IO ()
processInputFile inputFile knownBoundaryText targetBoundaryText = do
        putStrLn "Loading database and input files..."

        let knownBoundary = read knownBoundaryText :: Int
        let targetBoundary = read targetBoundaryText :: Int

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

        let summaryHeader = makeHeader inputFile knownBoundary targetBoundary

        let statisticsHeader = makeTable (
                zip5
                        [
                                "Number of input words",
                                "Number of known words",
                                "Number of to-be-known words",
                                "Number of ignored words",
                                "Number of not-found words"
                        ]
                        [
                                inputSize,
                                knownSize,
                                toBeKnownSize,
                                ignoredSize,
                                notFoundSize
                        ]
                        [
                                fromIntegral inputSize / fromIntegral inputSize,
                                fromIntegral knownSize / fromIntegral inputSize,
                                fromIntegral toBeKnownSize / fromIntegral inputSize,
                                fromIntegral ignoredSize / fromIntegral inputSize,
                                fromIntegral notFoundSize / fromIntegral inputSize
                        ]
                        [
                                "normal",
                                "normal",
                                toBeKnownAnchor,
                                ignoredAnchor,
                                notFoundAnchor
                        ]
                        [
                                "",
                                "",
                                toBeKnownAnchor,
                                ignoredAnchor,
                                notFoundAnchor
                        ]
                        )

        putStrLn "Colorizing input based on given boundaries..."

        let colorizedLines = map (linify . process categories) (lines inputText)
        let outputFilename = "index.html"
        writeFile outputFilename (htmlize
                (summaryHeader
                ++ statisticsHeader
                ++ unlines colorizedLines
                ++ includeWordListSection "To Be Known Words" toBeKnownAnchor toBeKnownWordsInInput
                ++ includeWordListSection "Ignored Words" ignoredAnchor ignoredWordsInInput
                ++ includeWordListSection "Not Found Words" notFoundAnchor notFoundWordsInInput
                ))
        putStrLn (printf "Done. Output written to %s" outputFilename)

tokenize :: String -> Set.Set String
tokenize text = Set.fromList uppercaseWords
        where
                uppercaseWords = splitWhen (not . isAlpha) (map toUpper text)

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

linify :: String -> String
linify xs = wrapHtmlTag "p" xs []

htmlize :: String -> String
htmlize bodyText =
        wrapHtmlTag "html" htmlSection []
        where
                htmlSection =
                        wrapHtmlTag "head" headSection [] ++ wrapHtmlTag "body" bodyText []
                headSection =
                        wrapHtmlTag "link" "" [
                                ("rel", "stylesheet"),
                                ("type", "text/css"),
                                ("href", "style.css")
                                ]

includeWordListSection :: String -> String -> Set.Set String -> String
includeWordListSection name anchor wordSet =
        wrapHtmlTag "h1" headerText [] ++ wrapHtmlOrderedList listText
        where
                headerText = wrapHtmlTag "a" "" [("name", anchor)] ++ name
                listText = unlines $ map (wrapHtmlListItem . lowerWord) listEntries
                listEntries = Set.toAscList wordSet
                wrapHtmlOrderedList xs = wrapHtmlTag "ol" xs []
                wrapHtmlListItem xs = wrapHtmlTag "li" (linkedWord xs) []
                linkedWord xs = wrapHtmlTag "a" xs [("href", "http://www.vocabulary.com/dictionary/" ++ xs)]
                lowerWord = map toLower

wrapHtmlTag :: String -> String -> [(String, String)] -> String
wrapHtmlTag tag value attributes =
        "<" ++ tag ++ " " ++ attributeText ++ ">" ++ value ++ "</" ++ tag ++ ">\n"
        where
                attributeText = unwords [aName ++ "=" ++ aValue | (aName,aValue) <- attributes]

makeTable :: [(String, Int, Double, String, String)] -> String
makeTable quints =
        "<table>"
        ++ unlines (map
                (\(name, value, percent, style, href) ->
                        "<tr><td class=\"" ++ style ++ "\">"
                        ++ name
                        ++ "</td><td class=\"stat\">"
                        ++ (if (not . null) href then "<a href=\"#" ++ href ++ "\">" else "")
                        ++ show value
                        ++ (if (not . null) href then "</a>" else "")
                        ++ "</td><td class=\"stat\">"
                        ++ printf "%6.2f" (percent * 100.0)
                        ++ " %</td></tr>")
                quints)
        ++ "</table><hr/>"

makeHeader :: FilePath -> Int -> Int -> String
makeHeader = printf "<h1>%s</h1>\n<p>Known words: %dK, target words: %dK.</p>"

popWord :: String -> String
popWord = takeWhile isAlpha

skipWord :: String -> String
skipWord = dropWhile isAlpha

copyTillWord :: String -> String
copyTillWord = takeWhile (not . isAlpha)

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
        | isWordInCategory upperWord (categories !! 1) = decorate xs toBeKnownAnchor
        | isWordInCategory upperWord (categories !! 2) = decorate xs ignoredAnchor
        | otherwise = decorate xs notFoundAnchor
        where
                upperWord = map toUpper xs

isWordInCategory :: String -> Set.Set String -> Bool
isWordInCategory = Set.member

decorate :: String -> String -> String
decorate xs cssClass = "<span class=\"" ++ cssClass ++ "\">" ++ xs ++ "</span>"
