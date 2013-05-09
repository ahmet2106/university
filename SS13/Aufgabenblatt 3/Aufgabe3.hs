{-
Copyright (C) 2013 Ahmet Topal <ahmet-topal.com>
licence: CC BY-SA
-}

import Data.Char
import Data.List

-- a)
ersetze :: String -> String -> String -> String
ersetze such ersetz text = sucheErsetze such ersetz text 0 where
	sucheErsetze such ersetz text pos = if pos < length text
	then	
		if isPrefixOf such (snd (splitAt pos text))
		then sucheErsetze such ersetz (fst (splitAt pos text) ++ ersetz ++ snd (splitAt (pos + (length ersetz)) text)) (pos+(length ersetz))
		else sucheErsetze such ersetz text (pos+1)
	else text

-- b)
ersetzeJedesMalAnders :: String -> [String] -> String -> String
ersetzeJedesMalAnders such ersetz text = sucheErsetze such ersetz text 0 0 where
	sucheErsetze such ersetz text pos index = if pos < length text
	then
		if isPrefixOf such (snd (splitAt pos text))
		then sucheErsetze such ersetz (fst (splitAt pos text) ++ (ersetz !! index) ++ snd (splitAt (pos + (length (ersetz !! index))) text)) (pos+(length (ersetz !! index))) (index+1)
		else sucheErsetze such ersetz text (pos+1) index
	else text

-- c)
-- ersetzeMehrere :: [String] -> [String] -> String -> String