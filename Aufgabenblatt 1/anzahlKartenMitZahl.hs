{-
Copyright (C) 2013 Ahmet Topal <ahmet-topal.com>
licence: CC BY-SA
-}

-- import System.Random and randomInts from prg2.hs
import System.Random
randomInts a b =  randomRs (a,b) (mkStdGen 1)

-- overwrite this Int to set maxIndex m
maxIndex = 3

-- matrix generator for (a, b) = randomInts 1 maxIndex (a+b)
matrix :: Int -> Int -> Int
matrix a b = do
    let r = a+b
    randomInts 1 maxIndex !! r

-- check if matrix from to is equal to x return 1 else 0 to count and add
isEqualMatrix :: Int -> Int -> Int -> Int
isEqualMatrix from to x = do
    let m = matrix from to
    if m == x then 1 else 0

-- recursive matrix check calling isEqualTo from to x for adding each matrix (1, 1) -> matrix (m, m)
recMatrix :: Int -> Int -> Int -> Int
recMatrix from to x
    | from <= maxIndex && to <= maxIndex = do
        let r = to+1
        isEqualMatrix from to x + recMatrix from r x
    | from <= maxIndex && to > maxIndex = do
        let r = from+1
        recMatrix r 1 x
    | otherwise = 0

-- main call anzahlKartenMitZahl with Int z to check in recMatrix 1 1 z
anzahlKartenMitZahl :: Int -> Int
anzahlKartenMitZahl z = do
    recMatrix 1 1 z
