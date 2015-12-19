import System.Environment (getArgs)
import Data.Char (isAlpha, toUpper)
import qualified Data.Set as Set
import Text.Printf (printf)
import Data.List.Split (splitWhen)

maxDBNumber :: Int
maxDBNumber = 34

outputFilename :: String
outputFilename = "colorized.html"

main :: IO ()
main = do
        args <- getArgs
        case args of
                [inputFile, knownBoundary, targetBoundary] ->
                        processInputFile inputFile knownBoundary targetBoundary
                _ -> putStrLn "Use parameters: <inputfile> <known> <to-be-known>"

processInputFile :: FilePath -> String -> String -> IO ()
processInputFile inputFile knownBoundaryText targetBoundaryText = do
        putStrLn "Colorizing input based on given boundaries..."
        putStrLn (printf "Upper boundary of known words: %s-K" knownBoundaryText)
        putStrLn (printf "Upper boundary of to-be-known words: %s-K" targetBoundaryText)
        let knownBoundary = read knownBoundaryText :: Int
        let targetBoundary = read targetBoundaryText :: Int
        inputText <- readFile inputFile
        let inputSet = tokenize inputText
        knownWordSet <- loadSetFromRange 1 knownBoundary
        targetWordSet <- loadSetFromRange (knownBoundary + 1) targetBoundary
        niceToHaveWordSet <- loadSetFromRange (targetBoundary + 1) maxDBNumber
        let categories = [
                Set.intersection inputSet knownWordSet,
                Set.intersection inputSet targetWordSet,
                Set.intersection inputSet niceToHaveWordSet]
        let colorizedLines = map (linify . process categories) (lines inputText)
        writeFile outputFilename (htmlize (unlines colorizedLines))
        putStrLn (printf "Output written to %s" outputFilename)

tokenize :: String -> Set.Set String
tokenize text = Set.fromList uppercaseWords
        where
                uppercaseWords = splitWhen (not . isAlpha) (map toUpper text)

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
        | map toUpper xs `elem` (categories !! 2) = decorate xs "unknown"
        | otherwise = decorate xs "not-found"

decorate :: String -> String -> String
decorate xs cssClass = "<span class=\"" ++ cssClass ++ "\">" ++ xs ++ "</span>"
