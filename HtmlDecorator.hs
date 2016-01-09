module HtmlDecorator (
linify
,wrapInHtmlBody
,generateWordListSection
,wrapInHtmlSpan
,generateStatisticsHeader
,generateTable
) where

import qualified Data.Set    as Set
import           Data.Char   (toLower)
import           Text.Printf (printf)

wrapInHtmlTag :: String -> String -> [(String, String)] -> String
wrapInHtmlTag tag value attributes =
        "<" ++ tag ++ " " ++ attributeText ++ ">" ++ value ++ "</" ++ tag ++ ">\n"
        where
                attributeText = unwords [aName ++ "=" ++ aValue | (aName,aValue) <- attributes]

linify :: String -> String
linify xs = wrapInHtmlTag "p" xs []

wrapInHtmlBody :: String -> String
wrapInHtmlBody bodyText =
        wrapInHtmlTag "html" htmlSection []
        where
                htmlSection =
                        wrapInHtmlTag "head" headSection [] ++ wrapInHtmlTag "body" bodyText []
                headSection =
                        wrapInHtmlTag "link" "" [
                                ("rel", "stylesheet"),
                                ("type", "text/css"),
                                ("href", "style.css")
                                ]

generateWordListSection :: String -> String -> Set.Set String -> String
generateWordListSection name anchor wordSet =
        wrapInHtmlTag "h1" headerText [] ++ wrapHtmlOrderedList listText
        where
                headerText = wrapInHtmlTag "a" "" [("name", anchor)] ++ name
                listText = unlines $ map (wrapHtmlListItem . lowerWord) listEntries
                listEntries = Set.toAscList wordSet
                wrapHtmlOrderedList xs = wrapInHtmlTag "ol" xs []
                wrapHtmlListItem xs = wrapInHtmlTag "li" (linkedWord xs) []
                linkedWord xs = wrapInHtmlTag "a" xs [("href", "http://www.vocabulary.com/dictionary/" ++ xs)]
                lowerWord = map toLower

generateTable :: [(String, Int, Double, String, String)] -> String
generateTable quints =
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
        ++ "</table><hr/>"

wrapInHtmlSpan :: String -> String -> String
wrapInHtmlSpan xs cssClass = "<span class=\"" ++ cssClass ++ "\">" ++ xs ++ "</span>"

generateStatisticsHeader :: FilePath -> Int -> Int -> String
generateStatisticsHeader = printf "<h1>%s</h1>\n<p>Known words: %dK, target words: %dK.</p>"
