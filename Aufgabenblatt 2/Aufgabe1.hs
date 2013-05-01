{-
Copyright (C) 2013 Ahmet Topal <ahmet-topal.com>
licence: CC BY-SA
-}

-- untersteZahl
untersteZahl :: Integer
-- obersteZahl
obersteZahl :: Integer -> Bool
-- direktUeber
direktUeber :: Integer -> Integer

-- untersteZahl
untersteZahl = 9

-- obersteZahl
obersteZahl 10 = True
obersteZahl _ = False

-- direktUeber
direktUeber 9 = 8
direktUeber 8 = 1
direktUeber 1 = 18
direktUeber 18 = 11
direktUeber 11 = 5
direktUeber 5 = 3
direktUeber 3 = 20
direktUeber 20 = 10
direktUeber 10 = error "dies ist die oberste Zahl"
direktUeber _ = error "diese Zahl liegt nicht im Stapel"

-- stapelEnthaeltZahl
stapelEnthaeltZahl :: Integer -> Bool
stapelEnthaeltZahl x = ownStapelEnthaeltZahl x untersteZahl where
    ownStapelEnthaeltZahl x y   | x == y = True
                                | otherwise = do
                                    if obersteZahl y == True
                                        then if x == y
                                            then True
                                            else False
                                        else ownStapelEnthaeltZahl x (direktUeber y)

-- summiereStapel
summiereStapel :: Integer
summiereStapel = ownSummiereStapel untersteZahl where
    ownSummiereStapel x | obersteZahl x == False = x + ownSummiereStapel (direktUeber x)
	                    | otherwise = x

-- liegtDarueber
liegtDarueber :: Integer -> Integer -> Bool
liegtDarueber a b  -- a ueber b
	| stapelEnthaeltZahl a == False || stapelEnthaeltZahl b == False = False
	| a == b = False
	| untersteZahl == a = False
	| obersteZahl b == True = False
	| direktUeber b == a = True
	| otherwise = ownLiegtDarueber a (direktUeber b) where
	    ownLiegtDarueber a b    | b == a = True
                                | obersteZahl b == True = False
                                | otherwise = ownLiegtDarueber a (direktUeber b)