{-|
    Module      : Types
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat alle type-declaraties, instanties en type-gerelateerde functies, zodat we deze makkelijk in meerdere modules kunnen importeren.
-}

{-# LANGUAGE TypeApplications #-}

module Types ( Beats, Hz, Samples, Seconds, Semitones, Track, Ringtone
             , Tone(..), Octave(..), Duration(..), Note(..)
             , Sound, floatSound, intSound, (<+>), asFloatSound, asIntSound, getAsFloats, getAsInts
             , Instrument, instrument, Modifier, modifier, modifyInstrument, arrange
             ) where

import Data.Int (Int32)
import Util ( zipWithL, zipWithR )

type Pulse = [Float]
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float
type Ringtone = String

data Tone = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B deriving (Enum, Eq, Show)
data Octave = Zero | One | Two | Three | Four | Five | Six | Seven | Eight deriving (Enum, Eq, Show)
data Duration = Full | Half | Quarter | Eighth | Sixteenth | Thirtysecond | Dotted Duration deriving (Eq, Show)
data Note = Pause Duration | Note Tone Octave Duration deriving (Eq, Show)

data Sound = IntFrames [Int32] | FloatFrames [Float]
  deriving Show

floatSound :: [Float] -> Sound
floatSound = FloatFrames

intSound :: [Int32] -> Sound
intSound = IntFrames

instance Eq Sound where
  (FloatFrames xs) == (FloatFrames ys) = all ((<  0.001) . abs) $ zipWith (-) xs ys
  (IntFrames xs) == (IntFrames ys)     = all ((< 10    ) . abs) $ zipWith (-) xs ys
  _ == _                               = False

-- TODO Maak instances voor `Sound` voor `Semigroup` en `Monoid`. De monoid-operatie staat in dit geval voor het sequentieel (achter elkaar) combineren van audiofragmenten. Het is van belang dat `IntFrames` alleen met `IntFrames` worden gecombineerd, en dito voor `FloatFrames`. Bij twee verschillende gevallen moet je beiden naar hetzelfde formaat converteren, idealiter `FloatFrames`. Wat is een leeg audiofragment in deze context?
instance Semigroup Sound where
  {- |
  FloatFrames en IntFrames kan je gewoon concateneren.
  Als je FloatFrames en IntFrames wil concateneren, moet je eerst alle IntFrames converteren naar Floats met getAsFloats.
  -}
  (IntFrames x) <> (IntFrames y) = IntFrames (x ++ y)
  (FloatFrames x) <> (FloatFrames y) = FloatFrames (x ++ y)
  (FloatFrames x) <> (IntFrames y) = FloatFrames (x ++ getAsFloats (intSound y))
  (IntFrames x) <> (FloatFrames y) = FloatFrames (getAsFloats (intSound x) ++ y)

instance Monoid Sound where
  {- |
  mempty is een lijst zonder frames.
  -}
  mempty = IntFrames[]


-- TODO Maak een operator `(<+>)` die twee `Sound`s  tot een enkel `Sound` combineert door de geluiden tegelijk af te spreken. Dit mag door de frames als in een `zipWith (+)` samen te voegen, maar in dit geval wordt niet de kortste maar de langste lijst aangehouden (in dat geval wordt enkel het aanwezige geluid weergegeven). Je hoeft deze operator alleen op `FloatFrames` te matchen, de laatste regel converteert alles hierheen als een of beide argumenten in `IntFrames` staan.
(<+>) :: Sound -> Sound -> Sound
{- |
Als de lengte van x groter is, moet de uitput dezelfde lengte hebben, visa versa voor y.
We hebben hier al functies voor geschreven in Util.hs.
-}
(<+>) (FloatFrames x) (FloatFrames y)
  | length x >= length y = FloatFrames (zipWithL (+) x y)
  | length x < length y = FloatFrames (zipWithR (+) x y)
x <+> y = asFloatSound x <+> asFloatSound y

asFloatSound :: Sound -> Sound
asFloatSound (IntFrames fs) = floatSound $ map ( (/ fromIntegral (div (maxBound @Int32 ) 2 )) . fromIntegral ) fs
asFloatSound fframe = fframe

-- TODO Maak een functie `asIntSOund` die als inverse van `asFloatSound` fungeert.
asIntSound :: Sound -> Sound
{- |
Dit moet het tegenovergestelde zijn van asFloatSound.
( maxBound @Int32 `div` 2 ) is een erg groot getal waar de mee moeten vermenigvuldigen.
Daarna ronden we dit getal af, en doen dit voor elk element in de lijst met map.
Ten slotte pakken we alles weer netjes in in Sound.
-}
asIntSound (FloatFrames fs) = intSound $ map (\x -> round (fromIntegral ( maxBound @Int32 `div` 2 ) * x)) fs
asIntSound fframe = fframe

getAsFloats :: Sound -> [Float]
getAsFloats sound = case asFloatSound sound of
  (FloatFrames ffs) -> ffs
  _ -> error "asFloatSound did not return FloatFrames"

getAsInts :: Sound -> [Int32]
getAsInts sound = case asIntSound sound of
  (IntFrames ifs) -> ifs
  _ -> error "asIntSound did not return IntFrames"

type Track = (Instrument, [Note])

newtype Instrument = Instrument (Hz -> Seconds -> Pulse)

instrument :: (Hz -> Seconds -> Pulse) -> Instrument
instrument = Instrument

newtype Modifier = Modifier (Pulse -> Pulse)

modifier :: (Pulse -> Pulse) -> Modifier
modifier = Modifier

instance Semigroup Modifier where
  (Modifier m1) <> (Modifier m2) = Modifier $ m1 . m2

-- TODO Maak een functie `modifyInstrument) die een `Modifier` met een `Instrument` combineert. Gebruik een lambda om een nieuw instrument terug te geven, waarbij de functie in de modifier met de functie in het instrument gecomposed wordt.
modifyInstrument :: Instrument -> Modifier -> Instrument
{- |
Pakt het instrument en modifier uit zodat we deze in de functie kunnen gebruiken.
In de lambda pakt hij de Hz en Seconds uit het instrument, en maakt er een instrument van.
Zet dat instrument in de Modifier en pak alles weer in als Instrument
-}
modifyInstrument (Instrument inst) (Modifier mod) = Instrument (\x y -> mod $ inst x y)

-- TODO Maak een functie `arrange` die de functie in het meegegeven `Instrument` toepast op de frequentie en duur. Het resultaat wordt als `Sound` verpakt.
arrange :: Instrument -> Hz -> Seconds -> Sound
{- |
Pakt het instrument uit, zodat we deze in de functie kunnen gebruiken.
Maakt een nieuw instrument en pakt maakt er een Sound van.
Een instrument geeft een Pulse, een Pulse is een [Float], die we aan floatSound kunnen doorgeven.
-}
arrange (Instrument inst) h s = floatSound (inst h s)