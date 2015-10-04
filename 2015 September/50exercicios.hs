{-
    Ficha dos 50 exercícios publicados em Setembro sobre os quais incidirá a primeira avaliação.
-}

-- 1.

myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo x y =
    if (x <= y) then
        x : myEnumFromTo (x + 1) y
    else
        []

-- 2.

myEnumFromThenTo :: Int -> Int -> Int -> [Int]
myEnumFromThenTo x y z =
    if (x <= z) then
        x : myEnumFromThenTo (x + (y - x)) (y + (y - x)) z
    else
        []

-- 3.

myConcat :: [a] -> [a] -> [a]
myConcat a b =
    if (null a) then
        b
    else
        myConcat (init a) ((last a) : b)

-- 4.

myLast :: [a] -> a
myLast a =
    if (null a) then
        error "Can't get last element of empty list"
    else if (length a == 1) then
        a !! 0
    else
        myLast (tail a)

-- 5.

myInit :: [a] -> [a]
myInit b =
    if (null b) then
        []
    else
        reverse (tail (reverse b) )

-- 6.

myIndex :: [a] -> Int -> a
myIndex list index =
    if (index >= length list) then
        error "Index out of bounds"
    else if (index > 0) then
        myIndex (tail list) (index - 1)
    else
        head list

-- 7.


