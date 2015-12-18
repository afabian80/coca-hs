import Data.Char (isAlpha)

main :: IO ()
main = do
        let colorizedLines = map (linify . process) (lines subtitle)
        putStrLn $ htmlize (unlines colorizedLines)

linify :: String -> String
linify xs = "<p>" ++ xs ++ "</p>"

htmlize :: String -> String
htmlize xs = "<html>\n<body>\n" ++ xs ++ "\n</body>\n</html>"

colorForTargetWords :: String
colorForTargetWords = "LightGreen"

colorForUnknownWords :: String
colorForUnknownWords = "DarkSalmon"

subtitle :: String
subtitle = "2\n00:06:26,193 --> 00:06:28,926\nI'll have a smoke outside,\nor I'll fall asleep.\n\"Son of a beach\" - said Kolya."

wordsKnown :: [String]
wordsKnown = ["one", "two", "have", "smoke", "outside", "said"]

wordsTargeted :: [String]
wordsTargeted = ["fall", "beach"]

popWord :: String -> String
popWord = takeWhile isAlpha

skipWord :: String -> String
skipWord = dropWhile isAlpha

copyTillWord :: String -> String
copyTillWord = takeWhile (not . isAlpha)

process :: String -> String
process [] = []
process xs = colorize headWord ++ nonWordAfterHead ++ process tailWords
        where
                headWord = popWord xs
                nonWordAfterHead = copyTillWord $ skipWord xs
                tailWords = drop (length headWord + length nonWordAfterHead) xs

colorize :: String -> String
colorize [] = []
colorize xs
        | xs `elem` wordsKnown = xs
        | xs `elem` wordsTargeted = decorate xs colorForTargetWords
        | otherwise = decorate xs colorForUnknownWords

decorate :: String -> String -> String
decorate xs tag = "<span style=\"background-color:" ++ tag ++ "\">" ++ xs ++ "</span>"
