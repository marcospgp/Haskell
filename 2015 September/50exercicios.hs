{-
    Ficha dos 50 exercícios publicados em Setembro sobre os quais incidirá a primeira avaliação.
-}

import Data.List
import Data.Char

-- 1.

myEnumFromTo :: Int -> Int -> [Int]
myEnumFromTo x y
    | x <= y = x : myEnumFromTo (x + 1) y
    | otherwise = []

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
myInit (x : xs)
    | null xs = []
    | otherwise = x : myInit xs

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

myReverse :: [a] -> [a]
myReverse (x : xs)
    | null xs = x : []
    | otherwise = (myReverse xs) ++ (x : [])

-- 8.

myTake :: Int -> [a] -> [a]
myTake a (x : xs)
    | a >= (length xs + 1) = x : xs
    | a == 0 = []
    | otherwise = x : (myTake (a - 1) xs)

-- 9.

myDrop :: Int -> [a] -> [a]
myDrop a (x : xs)
    | a >= (length xs + 1) = []
    | a > 0 = myDrop (a - 1) xs
    | a == 0 = x : xs

-- 10.

myZip :: [a] -> [b] -> [(a,b)]
myZip (x : xs) (y : ys)
    | (null xs || null ys) = [(x,y)]
    | otherwise = (x,y) : myZip xs ys

-- 11.

myElem :: Eq a => a -> [a] -> Bool
myElem a (x : xs)
    | (a == x) = True
    | null xs = False
    | otherwise = myElem a xs

-- 12.

myReplicate :: Int -> a -> [a]
myReplicate x y
    | (x == 0) = []
    | otherwise = y : myReplicate (x - 1) y

-- 13.

myIntersperse :: a -> [a] -> [a]
myIntersperse a (x : xs)
    | null xs = x : []
    | otherwise = x : a : myIntersperse a xs

-- 14.

myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup l@(x : xs) =
    let
        (grouped, remainder) = span (== x) l
    in
        grouped : myGroup remainder

-- 15.

myJoin :: [[a]] -> [a]
myJoin [] = []
myJoin (x : xs) = x ++ myJoin (xs)

-- 16.

myInits :: [a] -> [[a]]
myInits [] = [[]]
myInits x = (myInits (init x)) ++ [x]

-- 17.

myTails :: [a] -> [[a]]
myTails [] = [[]]
myTails x = x : myTails (tail x)

-- 18.

myIsPrefixOf :: Eq a => [a] -> [a] -> Bool
myIsPrefixOf x y
    | null y = False
    | x == y = True
    | otherwise = myIsPrefixOf x (init y)

-- 19.

myIsSuffixOf :: Eq a => [a] -> [a] -> Bool
myIsSuffixOf x y
    | null y = False
    | x == y = True
    | otherwise = myIsSuffixOf x (tail y)

-- 20.

myIsSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myIsSubsequenceOf [] [] = True
myIsSubsequenceOf x [] = False
myIsSubsequenceOf [] x = True
myIsSubsequenceOf l@(x : xs) (y : ys)
    | x == y = myIsSubsequenceOf xs ys
    | otherwise = myIsSubsequenceOf l ys

-- 21.

myElemIndices :: Eq a => a -> [a] -> [Int]
myElemIndices a x =
    let
        aux :: Eq a => a -> Int -> [a] -> [Int]
        aux a b [] = []
        aux a b (x : xs)
            | a == x = b : aux a (b + 1) xs
            | otherwise = aux a (b + 1) xs
    in
        aux a 0 x

-- 22.

myNub :: Eq a => [a] -> [a]
myNub x =
    let
        myNubAux :: Eq a
                 => [a] -- The list we want to take elements from (without repeating)
                 -> [a] -- The list of elements we have taken already
                 -> [a]
        myNubAux [] [] = []
        myNubAux [] x = x
        myNubAux (x : xs) l
            | elem x l = myNubAux xs l
            | otherwise = myNubAux xs (l ++ [x])
    in
        myNubAux x []

-- 23.

myDelete :: Eq a => a -> [a] -> [a]
myDelete x [] = []
myDelete x (y : ys)
    | (x == y) = ys
    | otherwise = y : myDelete x ys

-- 24.

myShave :: Eq a => [a] -> [a] -> [a]
myShave [] x = []
myShave x [] = x
myShave x (y : ys) = myShave (myDelete y x) ys
    where
        myDelete x [] = []
        myDelete x (y : ys)
            | (x == y) = ys
            | otherwise = y : myDelete x ys

-- 25.

myUnion :: Eq a => [a] -> [a] -> [a]
myUnion [] x = x
myUnion x [] = x
myUnion x (y : ys)
    | elem y x = myUnion x ys
    | otherwise = myUnion (x ++ [y]) ys

-- 26.

myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect [] _ = []
myIntersect x [] = x
myIntersect (x : xs) y
    | (elem x y) = x : myIntersect xs y
    | otherwise = myIntersect xs y

-- 27.

myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x l@(y : ys)
    | (y >= x) = x : l
    | otherwise = y : myInsert x ys

-- 28.

myMaximum :: Ord a => [a] -> a
myMaximum [x] = x
myMaximum (x : xs) = myMaximumAux x xs
    where
        myMaximumAux :: Ord a
                     => a   -- Biggest element found so far
                     -> [a] -- Array or elements to search through
                     -> a
        myMaximumAux x [] = x
        myMaximumAux x (y : ys)
            | (x >= y) = myMaximumAux x ys
            | otherwise = myMaximumAux y ys

-- 29.

myMinimum :: Ord a => [a] -> a
myMinimum [x] = x
myMinimum (x : xs)
    | (x < head xs) = myMinimum (x : tail xs)
    | otherwise = myMinimum xs

-- 30.

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

-- 31.

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

-- 32.

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs)
    | x = myAnd xs
    | otherwise = False

-- 33.

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs)
    | x = True
    | otherwise = myOr xs

-- 34.

myUnwords :: [String] -> String
myUnwords [] = ""
myUnwords [x] = x
myUnwords (x : xs) = x ++ " " ++ myUnwords xs

-- 35.

myUnlines :: [String] -> String
myUnlines [] = ""
myUnlines (x : xs) = x ++ "\n" ++ myUnlines xs

-- 36.

pMaior :: Ord a => [a] -> Int
pMaior [] = error "Can't get biggest element of an empty list"
pMaior x =
    let
        pMaiorAux [x] y z = y -- y: Biggest index so far z: Current index
        pMaiorAux (x : xs) y z
            | (x > head xs) = pMaiorAux (x : tail xs) z (z + 1)
            | otherwise = pMaiorAux xs (z + 1) (z + 1)
    in
        pMaiorAux x 0 0

-- 37.

temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (x : xs)
    | elem x xs = True
    | otherwise = temRepetidos xs

-- 38.

algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos x =
    let
        algarismosAux :: [Char] -> [Char] -> [Char]
        algarismosAux [] y = y
        algarismosAux (x : xs) y
            | elem x ['0','1','2','3','4','5','6','7','8','9'] = algarismosAux xs (y ++ [x])
            | otherwise = algarismosAux xs y
    in
        algarismosAux x []

-- 39.

posImpares :: [a] -> [a]
posImpares [] = []
posImpares [x] = []
posImpares (x : xs) = head xs : (posImpares (tail xs))

-- 40.

posPares :: [a] -> [a]
posPares [] = []
posPares [x] = [x]
posPares (x : xs) = x : (posPares (tail xs))

-- 41.

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x : xs)
    | (x <= head xs) = isSorted xs
    | otherwise = False

-- 42.

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort [x] = [x]
iSort (x : xs) = insert x (iSort xs)

-- 43.

menor :: String -> String -> Bool
menor [x] [y] = (toLower x < toLower y)
menor x "" = False
menor "" x = True
menor (x : xs) (y : ys)
    | (toLower x <= toLower y) = menor xs ys
    | otherwise = False

-- 44.

elemMSet :: Eq a => a -> [(a, Int)] -> Bool
elemMSet x [] = False
elemMSet x (y : ys)
    | (x == fst y) = True
    | otherwise = elemMSet x ys

-- 45.

lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet (x : xs) = snd x + lengthMSet xs

-- 46.

converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet l@(x : xs)
    | (snd x > 1) = fst x : (converteMSet ((fst x, (snd x) - 1) : xs))
    | otherwise = fst x : (converteMSet xs)

-- 47.

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x, 1)]
insereMSet x ((a,b) : ys)
    | (x == a) = (a, (b + 1)) : ys
    | otherwise = (a,b) : insereMSet x ys

-- 48.

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,b) : ys)
    | (x == a && b == 1) = ys
    | (x == a) = (a, (b - 1)) : ys
    | otherwise = (a,b) : (removeMSet x ys)

-- 49.

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet x = aux x []
    where
        -- Takes a string to build and an empty list and starts
        -- building the string onto the empty list recursively
        aux [] y = y
        aux (x : xs) y = aux xs (insereMSet x y)

        insereMSet x [] = [(x, 1)]
        insereMSet x ((a,b) : ys)
            | (x == a) = (a, (b + 1)) : ys
            | otherwise = (a,b) : (insereMSet x ys)

-- 50.

somaPares :: [Int] -> Int
somaPares [] = 0
somaPares (x : xs)
    | (x `mod` 2) == 0 = x + (somaPares xs)
    | otherwise = somaPares xs

-- Acabei!! Queria agradecer ao sborting
