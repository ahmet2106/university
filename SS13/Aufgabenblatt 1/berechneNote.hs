{-
Copyright (C) 2013 Ahmet Topal <ahmet-topal.com>
licence: CC BY-SA
-}

-- main call berechneNote with Klausurpunkte Bonuspunkte to get mark
berechneNote :: Float -> Float -> Float
berechneNote k b
	| k > 100	= error "Zu viele Klausurpunkte"
	| k < 0		= error "Zu wenig Klausurpunkte"
	| b > 20	= error "Zu viele Bonuspunkte"
	| b < 0		= error "Zu wenig Bonuspunkte"
	| k + b < 50	= 5.0
	| k + b < 54	= 4.0
	| k + b < 58	= 3.7
	| k + b < 62	= 3.3
	| k + b < 66	= 3.0
	| k + b < 70	= 2.7
	| k + b < 74	= 2.3
	| k + b < 78	= 2.0
	| k + b < 82	= 1.7
	| k + b < 86	= 1.3
	| otherwise	= 1.0