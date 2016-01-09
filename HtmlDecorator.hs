module HtmlDecorator (
linify
,wrapInHtmlBody
,generateWordListSection
,wrapInHtmlSpan
,generateStatisticsHeader
,generateTable
,TableData (TableRow)
) where

import qualified Data.Set    as Set
import           Data.Char   (toLower)
import           Text.Printf (printf)

data TableData = TableRow String Int Int String String deriving (Show, Read)

wrapInHtmlTag :: String -> [(String, String)] -> String -> String
wrapInHtmlTag tag attributes text =
        "<" ++ tag ++ " " ++ attributeText ++ ">" ++ text ++ "</" ++ tag ++ ">\n"
        where
                attributeText = unwords [aName ++ "=" ++ aValue | (aName,aValue) <- attributes]

linify :: String -> String
linify = wrapInHtmlTag "p" []

wrapInHtmlBody :: String -> String
wrapInHtmlBody bodyText =
        wrapInHtmlTag "html" [] htmlSection
        where
                htmlSection =
                        wrapInHtmlTag "head" [] headSection ++ wrapInHtmlTag "body" [] bodyText
                headSection =
                        wrapInHtmlTag "link" [
                                ("rel", "stylesheet"),
                                ("type", "text/css"),
                                ("href", "style.css")
                                ] ""

generateWordListSection :: String -> String -> Set.Set String -> String
generateWordListSection sectionName anchor wordSet =
        wrapInHtmlTag "h1" [] headerText ++ wrapHtmlOrderedList listText
        where
                headerText = wrapInHtmlTag "a" [("name", "\"" ++ anchor ++ "\"")] "" ++ sectionName
                listText = unlines $ map (wrapHtmlListItem . lowerWord) listEntries
                listEntries = Set.toAscList wordSet
                wrapHtmlOrderedList = wrapInHtmlTag "ol" []
                wrapHtmlListItem xs = wrapInHtmlTag "li" [] (linkedWord xs)
                linkedWord xs = wrapInHtmlTag "a" [("href", "http://www.vocabulary.com/dictionary/" ++ xs)] xs
                lowerWord = map toLower

generateTable :: [TableData] -> String
generateTable ds = wrapInHtmlTag "table" [] (unlines (map tableDataToString ds))

tableDataToString :: TableData -> String
tableDataToString (TableRow name value allWords style anchor) =
        wrapInHtmlTag "tr" [] tableRowText
        where
                tableRowText =
                        wrapInHtmlTag "td" [("class", style)] name
                        ++ "<td class=\"stat\">"
                        ++ (if (not . null) anchor then "<a href=\"#" ++ anchor ++ "\">" else "")
                        ++ show value
                        ++ (if (not . null) anchor then "</a>" else "")
                        ++ "</td><td class=\"stat\">"
                        ++ printf "%6.2f" (percent * 100.0)
                        ++ " %</td>"
                percent :: Double
                percent = fromIntegral value / fromIntegral allWords

wrapInHtmlSpan :: String -> String -> String
wrapInHtmlSpan xs cssClass = "<span class=\"" ++ cssClass ++ "\">" ++ xs ++ "</span>"

generateStatisticsHeader :: FilePath -> Int -> Int -> String
generateStatisticsHeader = printf "<h1>%s</h1>\n<p>Known words: %dK, target words: %dK.</p>"
