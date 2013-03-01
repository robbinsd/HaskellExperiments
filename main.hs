module Main where
import Matrix

main = do
    putStrLn "Running Main..."
    let asdf = emptyMatrix
    putStrLn (show asdf)
    let asdf2 = matrixFromArray [2,1] (0,0)
    putStrLn (show asdf2)
    let combined = (++) "Size of asdf2: " . show . matrixSize $ asdf2 in 
        putStrLn combined
        
    putStrLn "Done!"