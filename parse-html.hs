import Data.Char (isAlpha, toUpper)
import Data.List (isInfixOf)

main :: IO ()
--main = print $ parse "<html>Hello, <bold>world</bold>!</html>"
main = interact parse

parse :: String -> String
parse = parseRecurse False


parseRecurse :: Bool -> String -> String
parseRecurse _ [] = []
parseRecurse inBody xs = headWord ++ keepHtmlTag ++ keepNonWordChars ++ parseRecurse bodyReached remaining
        where
                headWord =
                        if inBody
                                then map toUpper $ takeWhile isAlpha xs
                                else takeWhile isAlpha xs
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
                bodyReached = inBody || bodyInHtmlTag
                bodyInHtmlTag = "<body" `isInfixOf` keepHtmlTag
