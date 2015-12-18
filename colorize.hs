import System.Environment (getArgs)
import Data.Char (isAlpha, toUpper)
import qualified Data.Set as Set

main :: IO ()
main = do
        args <- getArgs
        case args of
                [inputFile] -> processInputFile inputFile
                _ -> putStrLn "Error: input argument missing"

processInputFile :: FilePath -> IO ()
processInputFile inputFile = do
        inputText <- readFile inputFile
        knownWordsText <- readFile "cocadb/knownwords.txt"
        targetedWordsText <- readFile "cocadb/targetedwords.txt"
        niceToHaveWordsText <- readFile "cocadb/nicetohavewords.txt"
        let knownWordSet = Set.fromList (words knownWordsText)
        let targetWordSet = Set.fromList (words targetedWordsText)
        let niceToHaveWordSet = Set.fromList (words niceToHaveWordsText)
        let categories = [knownWordSet, targetWordSet, niceToHaveWordSet]
        let colorizedLines = map (linify . process categories) (lines inputText)
        putStrLn $ htmlize (unlines colorizedLines)

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
