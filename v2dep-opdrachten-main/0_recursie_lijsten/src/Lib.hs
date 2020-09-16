module Lib
    ( ex1, ex2, ex3, ex4, ex5, ex6, ex7
    ) where

-- Schrijf een functie die de som van een lijst getallen berekent, net als de `product` functie uit de les.
ex1 :: [Int] -> Int
-- ^ Pattern match op een lege lijst
ex1 [] = 0
-- ^ Voeg recursief het eerste element toe uit de tail van de 'vorige' lijst.
ex1 list = head list + ex1 (tail list)

-- Schrijf een functie die alle elementen van een lijst met 1 ophoogt; bijvoorbeeld [1,2,3] -> [2,3,4]. Het is voor deze opgave nog niet de bedoeling dat je hogere-orde functies gebruikt.
ex2 :: [Int] -> [Int]
-- ^ Pattern match op een lege lijst
ex2 [] = []
-- ^ Voeg recursief 1 toe aan de head van 'elke' lijst.
ex2 (x:xs) = (x + 1 : ex2 (xs))

-- Schrijf een functie die alle elementen van een lijst met -1 vermenigvuldigt; bijvoorbeeld [1,-2,3] -> [-1,2,-3]. Het is voor deze opgave nog niet de bedoeling dat je hogere-orde functies gebruikt.
ex3 :: [Int] -> [Int]
-- ^ Pattern match op een lege lijst
ex3 [] = []
-- ^ Haal recursief 1 af van de head uit 'elke' lijst.
ex3 (x:xs) = (x * (-1)) : ex3 (xs)

-- Schrijf een functie die twee lijsten aan elkaar plakt, dus bijvoorbeeld [1,2,3] en [4,5,6] combineert tot [1,2,3,4,5,6]. Maak hierbij geen gebruik van de standaard-functies, maar los het probleem zelf met (expliciete) recursie op. Hint: je hoeft maar door een van beide lijsten heen te lopen met recursie.
ex4 :: [Int] -> [Int] -> [Int]
-- ^ Pattern match op een lege lijst
ex4 list [] = list
ex4 list (x:xs) = ex4 (list ++ [x]) xs

-- Schrijf een functie die twee lijsten paarsgewijs bij elkaar optelt, dus bijvoorbeeld [1,2,3] en [4,5,6] combineert tot [1+4, 2+5, 3+6] oftewel [5,7,9]. Het is voor deze opgave nog niet de bedoeling dat je hogere-orde functies gebruikt.
ex5 :: [Int] -> [Int] -> [Int]
-- ^ Pattern match op een lege lijst
ex5 list [] = list
-- ^ Loop recursief over beide lijsten en voeg heads bij elkaar toe
ex5 (x:xs) (x2:xs2) = x + x2 : ex5 xs xs2

-- Schrijf een functie die twee lijsten paarsgewijs met elkaar vermenigvuldigt, dus bijvoorbeeld [1,2,3] en [4,5,6] combineert tot [1*4, 2*5, 3*6] oftewel [4,10,18]. Het is voor deze opgave nog niet de bedoeling dat je hogere-orde functies gebruikt.
ex6 :: [Int] -> [Int] -> [Int]
-- ^ Pattern match op een lege lijst
ex6 list [] = list
-- ^ Loop recursief over beide lijsten en vermenigvuldig heads
ex6 (x:xs) (x2:xs2) = x * x2 : ex6 xs xs2

-- Schrijf een functie die de functies uit opgave 1 en 6 combineert tot een functie die het inwendig prodct uitrekent. Bijvoorbeeld: `ex7 [1,2,3] [4,5,6]` -> 1*4 + 2*5 + 3*6 = 32.
ex7 :: [Int] -> [Int] -> Int
ex7 list list2 = ex1 (ex6 list list2) 
