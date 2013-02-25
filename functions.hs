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