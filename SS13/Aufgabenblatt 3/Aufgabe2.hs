{-
Copyright (C) 2013 Ahmet Topal <ahmet-topal.com>
licence: CC BY-SA
-}

import Data.Char
import Data.List

matrix :: [[Int]]
matrix = [[2,1,2]
		,[2,2,3]
		,[2,1,1]]

-- a)
anzahlKartenMitZahl :: Int -> [[Int]] -> Int
anzahlKartenMitZahl i [] = 0
anzahlKartenMitZahl i (x:xs) = (kartenZaehlen i x) + (anzahlKartenMitZahl i xs) where
	kartenZaehlen i [] = 0
	kartenZaehlen i (x:xs) = if i == x
		then 1 + (kartenZaehlen i xs)
		else 0 + (kartenZaehlen i xs)

-- b)
istGueltig :: [[Int]] -> Bool
istGueltig l = all (\x -> length x == length l) l

-- c)
fuelleMatrix :: Int -> [Int] -> [[Int]]
fuelleMatrix m zs = fuelleMatrixRek m zs [] 0 1 where
	fuelleMatrixRek m zs liste index counter = if counter <= (m*m)
		then
			if length liste < m
			then
				if index < length zs
				then fuelleMatrixRek m zs (liste ++ [zs !! index]) (index+1) (counter+1)
				else fuelleMatrixRek m zs (liste ++ [1]) (index+1) (counter+1)
			else
				[liste] ++ fuelleMatrixRek m zs [] index counter
		else
			[liste]