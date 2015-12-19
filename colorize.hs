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
        putStrLn (printf "Upper boundary of known words:       %3s-K" knownBoundaryText)
        putStrLn (printf "Upper boundary of to-be-known words: %3s-K" targetBoundaryText)
        putStrLn "\nLoading database and input files..."

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
        let outputFilename = "colorized.html"
        writeFile outputFilename (htmlize
                (statisticsHeader
                ++ unlines colorizedLines
                ++ includeWordList "To Be Known Words" toBeKnownAnchor toBeKnownWordsInInput
                ++ includeWordList "Ignored Words" ignoredAnchor ignoredWordsInInput
                ++ includeWordList "Not Found Words" notFoundAnchor notFoundWordsInInput
                ))
        putStrLn (printf "\nDone. Output written to %s" outputFilename)

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
        let wordSet = Set.fromList (words fileText)
        return wordSet

linify :: String -> String
linify xs = "<p>" ++ xs ++ "</p>"

htmlize :: String -> String
htmlize bodyText = "<html>\n<head>\n" ++
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"colors.css\">\n" ++
        "</head>\n<body>\n" ++ bodyText ++ "\n</body>\n</html>"

includeWordList :: String -> String -> Set.Set String -> String
includeWordList name anchor wordSet =
        "\n\n<h1>"
        ++ "<a name=\"" ++ anchor ++ "\"></a>"
        ++ name ++ "</h1>\n"
        ++ "<ol>\n"
        ++ unlines (map (\word -> "<li>" ++ map toLower word ++ "</li>") (Set.toAscList wordSet))
        ++ "</ol>"

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
        ++ "</table>"

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
        | map toUpper xs `elem` head categories = xs
        | map toUpper xs `elem` (categories !! 1) = decorate xs toBeKnownAnchor
        | map toUpper xs `elem` (categories !! 2) = decorate xs ignoredAnchor
        | otherwise = decorate xs notFoundAnchor

decorate :: String -> String -> String
decorate xs cssClass = "<span class=\"" ++ cssClass ++ "\">" ++ xs ++ "</span>"
