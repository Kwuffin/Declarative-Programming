{-|
    Module      : Types
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat enkele algemene polymorfe functies.
-}

module Util (zipWithL, zipWithR, comb, fst3, uncurry3) where

import Control.Applicative (liftA2)

-- | Version of ZipWith guaranteed to produce a list of the same length as the second input.
zipWithR :: (a -> b -> b) -> [a] -> [b] -> [b]
zipWithR _ _      []     = []
zipWithR _ []     bs     = bs
zipWithR f (a:as) (b:bs) = f a b : zipWithR f as bs

-- TODO Maak een `zipWithL` die als mirror-versie van `zipWithR` fungeert door de lengte gelijk te houden aan die van de eerste input in plaats van de tweede.
{- |
Pattern match op de eerste lijst
Als de eerste lijst leeg is, geef een lege lijst terug aan de functie.
Als de tweede lijst leeg is, geef de rest van de eerste lijst terug
-}
zipWithL :: (a -> b -> a) -> [a] -> [b] -> [a]
zipWithL _ [] _ = []
zipWithL _ xs2 [] = xs2
zipWithL f (x:xs) (x2:xs2) = f x x2 : zipWithL f xs xs2

-- | Use a given binary operator to combine the results of applying two separate functions to the same value. Alias of liftA2 specialised for the function applicative.
comb :: (b -> b -> b) -> (a -> b) -> (a -> b) -> a -> b
comb = liftA2

-- TODO Maak een functie `fst3` die het eerste element van een 3-tuple teruggeeft.
{- |
pakt het eerste element uit de tuple met '(a, _, _)', geef daarna 'a' terug.
-}
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- TODO Maak een functie `uncurry3` die een functie met drie argumenten transformeert naar een functie die een 3-tuple als argument neemt.
{- |
Neemt een functie en tuple als input.
Pakt de elementen uit de tuple met (a, b, c).
Voert daarna de functie uit met a, b en c als parameters.
-}
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c)= f a b c
