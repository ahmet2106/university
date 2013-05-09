{-
Copyright (C) 2013 Ahmet Topal <ahmet-topal.com>
licence: CC BY-SA
-}

import Data.Char
import Data.List

-- a)
quadrat x = x*x
aufgabe1a xs = map quadrat (filter (<500) xs)

-- b)
aufgabe1b :: [String] -> String
aufgabe1b [] = ""
aufgabe1b (x:xs) = if (toLower (x !! 1)) /= 'a'
	then (tail x) ++ aufgabe1b xs
	else aufgabe1b xs

-- c)
aufgabe1c :: [[String]] -> [String]
aufgabe1c [] = []
aufgabe1c (x:xs) = if length (toString x) >= 10
	then [toString x] ++ aufgabe1c xs
	else aufgabe1c xs
	where
		toString [] = ""
		toString x = (head x) ++ toString (tail x)

-- d)
aufgabe1d :: [String] -> [String]
aufgabe1d [] = []
aufgabe1d (x:xs) = if elem '!' x || elem '?' x then aufgabe1d xs else [x] ++ aufgabe1d xs
	where
		toUpperRek [] = ""
		toUpperRek (x:xs) = [toUpper x] ++ toUpperRek xs