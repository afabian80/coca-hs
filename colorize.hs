import           ColorizeHtml       (processInputFile)
import           System.Environment (getArgs)
import           System.Exit        (die)

main :: IO ()
main = do
        args <- getArgs
        case args of
                [inputFile, knownBoundary, targetBoundary] ->
                        processInputFile
                                inputFile
                                (read knownBoundary :: Int)
                                (read targetBoundary :: Int)
                _ -> die "Use parameters: <html-input-file> <K-known-number> <K-to-be-known-number>"
