import Data.Char (isAlpha, toUpper)

main :: IO ()
--main = print $ parse "<html>Hello, <bold>world</bold>!</html>"
main = interact parse

parse :: String -> String
parse = parseRecurse False


parseRecurse :: Bool -> String -> String
parseRecurse _ [] = []
parseRecurse inBody xs = headWord ++ keepHtmlTag ++ keepNonWordChars ++ parseRecurse inBody remaining
        where
                headWord = map toUpper $ takeWhile isAlpha xs
                afterHeadWord = drop (length headWord) xs

                keepHtmlTag = copyHtml afterHeadWord
                afterHtmlTag = drop (length keepHtmlTag) afterHeadWord
                copyHtml cs =
                        if head cs == '<'
                                then take (length htmlTag + 1) cs
                                else ""
                        where
                                htmlTag = takeWhile (/= '>') cs

                keepNonWordChars = takeWhile isNotAlphaOrTagStart afterHtmlTag
                isNotAlphaOrTagStart c = not (isAlpha c || c == '<')
                remaining = drop (length keepNonWordChars) afterHtmlTag
