bar x = x
doubleMe x = x + x
doubleMe2 x = 2*x
dist x y = sqrt (x*x+y*y)
doubleSmallNumber x = if x > 100 then x else x*2
doubleSmallNumber' x = doubleSmallNumber x + 1
myvar = 293

lucky 7 = "WUTUP"
lucky x = "boring"

funky x =
    square x + square x
    where square x = x*x

class TripleExclamation a where
    (!!!) :: Num b => [b] -> a -> [b]


instance TripleExclamation Int where
    (!!!) list index = list !! index : []

newtype IntList = IntList [Int]
instance TripleExclamation IntList where
    (!!!) list (IntList []) = []
    (!!!) list (IntList (firstIndex:otherIndices)) =
        (list !! firstIndex) :
        (list !!! (IntList otherIndices))

max' [x] = x
max' (x:xs) = max x (max' xs)

max'' xs =
    case xs of [x] -> x
               (x:xs) -> max x (max'' xs)
               
zeros n = take n (cycle [0])
ones n = take n (cycle [1])
smult scalar vector = [scalar * val | val <- vector]

data Matrix = MatFromArr [Float] (Int,Int) deriving Show

makeMatFromArr arr size@(m,n)
    | length arr == m*n = MatFromArr arr size
    {-| otherwise         = error "Array length does not match matrix dimensions!"-}

matIndicesValid (m,n) (i,j) = i >= 0 && i < m && j >= 0 && j < n
    
arrToMatIndices::(Int, Int) -> Int -> (Int, Int)
arrToMatIndices (m,n) arrIndex = (i,j) 
    where arrIndexFloat = fromIntegral arrIndex
          mFloat        = fromIntegral m
          i = arrIndex `mod` m
          j = floor (arrIndexFloat / mFloat)

matToArrIndices::(Int, Int) -> (Int, Int) -> Int
matToArrIndices (m,n) (i,j) 
    | matIndicesValid (m,n) (i,j) = arrIndex
    where arrIndex = j*m + i
    
matGetElem (MatFromArr arr size) indices = arr !! (matToArrIndices size indices)
               
data Vector = Vector3 Float Float Float deriving Show
(<+>) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (Vector3 (x1+x2) (y1+y2) (z1+z2))
dot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2
norm vec = sqrt (vec `dot` vec)