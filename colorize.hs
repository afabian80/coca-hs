import System.Environment (getArgs)
import Data.Char (isAlpha, toUpper)
import qualified Data.Set as Set
import Text.Printf (printf)

main :: IO ()
main = do
        args <- getArgs
        case args of
                [inputFile] -> processInputFile inputFile
                _ -> putStrLn "Error: input argument missing"

processInputFile :: FilePath -> IO ()
processInputFile inputFile = do
        inputText <- readFile inputFile
        knownWordSet <- loadSetFromRange 1 4
        targetWordSet <- loadSetFromRange 5 8
        niceToHaveWordSet <- loadSetFromRange 9 34
        let categories = [knownWordSet, targetWordSet, niceToHaveWordSet]
        let colorizedLines = map (linify . process categories) (lines inputText)
        putStrLn $ htmlize (unlines colorizedLines)

loadSetFromRange :: Int -> Int -> IO (Set.Set String)
loadSetFromRange lo hi = do
        let fileNames = generateFileNames lo hi
        sets <- mapM loadSet fileNames
        return (Set.unions sets)

generateFileNames :: Int -> Int -> [String]
generateFileNames lo hi =
        map (printf "cocadb/basewrd%02d.txt") [lo..hi]

loadSet :: String -> IO (Set.Set String)
loadSet path = do
        fileText <- readFile path
        let wordSet = Set.fromList (words fileText)
        return wordSet


linify :: String -> String
linify xs = "<p>" ++ xs ++ "</p>"

htmlize :: String -> String
htmlize xs = "<html>\n<body>\n" ++ xs ++ "\n</body>\n</html>"

colorForTargetWords :: String
colorForTargetWords = "LightGreen"

colorForUnknownWords :: String
colorForUnknownWords = "DarkSalmon"

colorForNiceToHaveWords :: String
colorForNiceToHaveWords = "Plum"

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
        nonWordAfterHead ++
        process categories tailWords
        where
                headWord = popWord xs
                nonWordAfterHead = copyTillWord $ skipWord xs
                tailWords = drop (length headWord + length nonWordAfterHead) xs

colorize :: String -> [Set.Set String] -> String
colorize [] _ = []
colorize xs categories
        | map toUpper xs `elem` head categories = xs
        | map toUpper xs `elem` (categories !! 1) = decorate xs colorForTargetWords
        | map toUpper xs `elem` (categories !! 2) = decorate xs colorForNiceToHaveWords
        | otherwise = decorate xs colorForUnknownWords

decorate :: String -> String -> String
decorate xs tag = "<span style=\"background-color:" ++ tag ++ "\">" ++ xs ++ "</span>"
