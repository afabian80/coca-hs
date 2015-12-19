import System.Environment (getArgs)
import Data.Char (isAlpha, toUpper)
import qualified Data.Set as Set
import Text.Printf (printf)
import Data.List.Split (splitWhen)
import System.Directory (getDirectoryContents)
import System.Posix.Files (getFileStatus, isRegularFile)

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
        putStrLn "\nLoading database and input files...\n"

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
        putStrLn (printf "Number of known words:         %8d" (length knownWordsInInput))
        putStrLn (printf "Number of to-be-known words:   %8d" (length toBeKnownWordsInInput))
        putStrLn (printf "Number of ignored words:       %8d" (length ignoredWordsInInput))

        putStrLn "\nColorizing input based on given boundaries..."
        let colorizedLines = map (linify . process categories) (lines inputText)
        let outputFilename = "colorized.html"
        writeFile outputFilename (htmlize (unlines colorizedLines))
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
htmlize xs = "<html>\n<head>\n" ++
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"colors.css\">\n" ++
        "</head>\n<body>\n" ++ xs ++ "\n</body>\n</html>"

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
        | map toUpper xs `elem` (categories !! 1) = decorate xs "to-be-known"
        | map toUpper xs `elem` (categories !! 2) = decorate xs "ignored"
        | otherwise = decorate xs "not-found"

decorate :: String -> String -> String
decorate xs cssClass = "<span class=\"" ++ cssClass ++ "\">" ++ xs ++ "</span>"
