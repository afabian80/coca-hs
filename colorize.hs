import           ColorizeText       (InputType, processInputFile)
import           System.Environment (getArgs)
import           System.Exit        (die)

main :: IO ()
main = do
        args <- getArgs
        case args of
                [inputFile, inputType, knownBoundary, targetBoundary] ->
                        processInputFile
                                inputFile
                                (read inputType :: InputType)
                                (read knownBoundary :: Int)
                                (read targetBoundary :: Int)
                _ -> die "Use parameters: <inputfile> <Html|Text> <known> <to-be-known>"
