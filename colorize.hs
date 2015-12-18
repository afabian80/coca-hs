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
        words1kText <- readFile "cocadb/basewrd01.txt"
        words5kText <- readFile "cocadb/basewrd05.txt"
        let knownWordSet = Set.fromList (words words1kText)
        let targetWordSet = Set.fromList (words words5kText)
        let colorizedLines = map (linify . process knownWordSet targetWordSet) (lines inputText)
        putStrLn $ htmlize (unlines colorizedLines)

linify :: String -> String
linify xs = "<p>" ++ xs ++ "</p>"

htmlize :: String -> String
htmlize xs = "<html>\n<body>\n" ++ xs ++ "\n</body>\n</html>"

colorForTargetWords :: String
colorForTargetWords = "LightGreen"

colorForUnknownWords :: String
colorForUnknownWords = "DarkSalmon"

popWord :: String -> String
popWord = takeWhile isAlpha

skipWord :: String -> String
skipWord = dropWhile isAlpha

copyTillWord :: String -> String
copyTillWord = takeWhile (not . isAlpha)

process :: Set.Set String -> Set.Set String -> String -> String
process _ _ [] = []
process knownWordSet targetWordSet xs =
        colorize headWord knownWordSet targetWordSet ++
        nonWordAfterHead ++
        process knownWordSet targetWordSet tailWords
        where
                headWord = popWord xs
                nonWordAfterHead = copyTillWord $ skipWord xs
                tailWords = drop (length headWord + length nonWordAfterHead) xs

colorize :: String -> Set.Set String -> Set.Set String -> String
colorize [] _ _ = []
colorize xs knownWordSet targetWordSet
        | map toUpper xs `elem` knownWordSet = xs
        | map toUpper xs `elem` targetWordSet = decorate xs colorForTargetWords
        | otherwise = decorate xs colorForUnknownWords

decorate :: String -> String -> String
decorate xs tag = "<span style=\"background-color:" ++ tag ++ "\">" ++ xs ++ "</span>"
