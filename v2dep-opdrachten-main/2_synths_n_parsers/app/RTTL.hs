{-|
    Module      : RTTL
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat een main-functie voor het lezen van user-supplied RTTL ringtones en het genereren van de behorende audio.
-}

module Main (main) where

import Data.Maybe (fromMaybe)
import Types (Instrument)
import Instruments (defaultInstrument , defaultSquare , defaultTriangle , pop , twisted , bass , kick , noise)
import Data (ppk)
import IO (playRTTL)

-- TODO Schrijf een main-functie die de gebruiker om een RTTL encoded String vraagt, het `instrumentMenu` print en vervolgens een getal overeenkomstig met een instrument. De string wordt met het gekozen element met `playRTTL` afgespeeld. Als er geen geldig instrument wordt meegegeven wordt `defaultInstrument` gepakt.
{- |
Vraagt de gebruiker om een RTTL-encoded String
rttl_string = gebruiker input

Print het instrumentMenu
Vraagt gebruiker om een instrument
instrKeuze = gebruiker input

Maak van string een int.
instr is de maybe Instrument die uit chooseInstrument komt.

Als instr Nothing is, voer RTTL met default waarden uit.
Als instr niet Nothing is, voer playRTTL uit met custom waarden.
-}
main :: IO ()
main = do 
    putStrLn "Geef een RTTL encoded String."
    rttl_string <- getLine

    putStrLn instrumentMenu
    putStrLn "Kies een instrument"
    instrKeuze <- getLine

    let intInstr = read instrKeuze
    let instr = chooseInstrument intInstr

    case instr of
        Nothing -> playRTTL defaultInstrument ppk
        Just x -> playRTTL x rttl_string

instrumentMenu :: String
instrumentMenu = unlines [ "1: sine"
                         , "2: square"
                         , "3: triangle"
                         , "4: pop"
                         , "5: twisted"
                         , "6: bass"
                         , "7: kick"
                         , "8: noise"
                         ]

-- TODO Schrijf een functie `chooseInstrument` die een `Int` interpreteert tot een `Maybe Instrument` volgens de tabel hierboven.
{- |
Voor de keuzes 1-8, geef een instrument.
Voor andere keuzes, geef Nothing.
-}
chooseInstrument :: Int -> Maybe Instrument
chooseInstrument i
    | i == 1 = Just defaultInstrument
    | i == 2 = Just defaultSquare
    | i == 3 = Just defaultTriangle
    | i == 4 = Just pop
    | i == 5 = Just twisted
    | i == 6 = Just bass
    | i == 7 = Just kick
    | i == 8 = Just noise
    | otherwise = Nothing
