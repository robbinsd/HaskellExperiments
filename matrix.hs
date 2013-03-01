module Matrix (Matrix, emptyMatrix, matrixFromArray, matrixSize) where

type MatrixSize = (Int,Int)
emptyMatrix :: Matrix Float
matrixFromArray :: Num a => [a] -> MatrixSize -> Matrix a
matrixSize :: Num a => Matrix a -> MatrixSize

data Matrix a = MatrixImpl [a] MatrixSize deriving Show -- opaque!
emptyMatrix = MatrixImpl [] (0,0)
matrixFromArray array size = MatrixImpl array size
matrixSize (MatrixImpl _ size) = size