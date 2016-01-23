import Data.Char (isAlpha)
import Data.List (isPrefixOf)

main :: IO ()
main = interact colorize

sampleText :: String
sampleText = "Egy cica, ket cica, harom cica, hajj. Megfogta az icipici egeret!"

sampleTagged :: String
sampleTagged = "Egy cica, <b>ket</b> cica, harom cica, hajj. Megfogta az icipici egeret!"

sampleHtml :: String
sampleHtml = "<html><head>Itt nem kene semmit cserelni!</head><body>Egy cica, <bold>ket</bold> cica, harom cica, hajj. Megfogta az icipici egeret!</body></html>"

colorize :: String -> String
colorize = colorizeRecurse False

colorizeRecurse :: Bool -> String -> String
colorizeRecurse _ [] = []
colorizeRecurse doChange text@(x:xs)
        | isAlpha x = decorate doChange (takeWhile isAlpha text) ++ colorizeRecurse doChange (dropWhile isAlpha text)
        | x == '<' = x : takeWhile (/= '>') xs ++ colorizeRecurse updatedDoChange (dropWhile (/= '>') xs)
        | otherwise = x : colorizeRecurse doChange xs
        where
                updatedDoChange = doChange || "body " `isPrefixOf` xs || "body>" `isPrefixOf` xs

decorate :: Bool -> String -> String
decorate doChange text = if doChange then "(" ++ text ++ ")" else text
