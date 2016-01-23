import Data.Char (isAlpha)

main :: IO ()
main = interact colorize

sampleText :: String
sampleText = "Egy cica, ket cica, harom cica, hajj. Megfogta az icipici egeret!"

sampleTagged :: String
sampleTagged = "Egy cica, <b>ket</b> cica, harom cica, hajj. Megfogta az icipici egeret!"

sampleHtml :: String
sampleHtml = "<html><head>Itt nem kene semmit cserelni!</head><body>Egy cica, <b>ket</b> cica, harom cica, hajj. Megfogta az icipici egeret!</body></html>"

colorize :: String -> String
colorize = colorizeRecurse False

colorizeRecurse :: Bool -> String -> String
colorizeRecurse _ [] = []
colorizeRecurse doChange text@(x:xs)
        | isAlpha x = decorate (takeWhile isAlpha text) ++ colorizeRecurse doChange (dropWhile isAlpha text)
        | x == '<' = x : takeWhile (/= '>') xs ++ colorizeRecurse doChange (dropWhile (/= '>') xs)
        | otherwise = x : colorizeRecurse doChange xs

decorate :: String -> String
decorate text = "(" ++ text ++ ")"
