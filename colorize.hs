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
        let knownWordSet = Set.fromList (words words1kText)
        let colorizedLines = map (linify . process knownWordSet) (lines inputText)
        putStrLn $ htmlize (unlines colorizedLines)

linify :: String -> String
linify xs = "<p>" ++ xs ++ "</p>"

htmlize :: String -> String
htmlize xs = "<html>\n<body>\n" ++ xs ++ "\n</body>\n</html>"

colorForTargetWords :: String
colorForTargetWords = "LightGreen"

colorForUnknownWords :: String
colorForUnknownWords = "DarkSalmon"

wordsTargeted :: [String]
wordsTargeted = ["fall", "beach"]

popWord :: String -> String
popWord = takeWhile isAlpha

skipWord :: String -> String
skipWord = dropWhile isAlpha

copyTillWord :: String -> String
copyTillWord = takeWhile (not . isAlpha)

process :: Set.Set String -> String -> String
process _ [] = []
process knownWordSet xs = colorize headWord knownWordSet ++ nonWordAfterHead ++ process knownWordSet tailWords
        where
                headWord = popWord xs
                nonWordAfterHead = copyTillWord $ skipWord xs
                tailWords = drop (length headWord + length nonWordAfterHead) xs

colorize :: String -> Set.Set String -> String
colorize [] _ = []
colorize xs knownWordSet
        | map toUpper xs `elem` knownWordSet = xs
        | xs `elem` wordsTargeted = decorate xs colorForTargetWords
        | otherwise = decorate xs colorForUnknownWords

decorate :: String -> String -> String
decorate xs tag = "<span style=\"background-color:" ++ tag ++ "\">" ++ xs ++ "</span>"
