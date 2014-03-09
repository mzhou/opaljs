{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeSynonymInstances #-}

module Main where

import Data.Array.Unboxed
import Data.Default (def)
import Data.Text (Text, pack, unpack)
import Data.Word (Word16, Word32)
import JavaScript.JQuery
import Language.Haskell.TH.Syntax (lift)

import Opal.Distance
import Opal.Fare.Train

stationOptions = $(lift $
    foldr (++) "" (map (\x -> "<option value=\"" ++ x ++ "\">" ++ x ++ "</option>") (map show stations))
    )

distanceList = $(lift $ concat [[trackDistance orig dest | dest <- stations] | orig <- stations])

distances = let
    numStations = fromIntegral $ 1 + fromEnum (maxBound :: Station)
    bounds = (0, numStations * numStations)
    in
    listArray bounds distanceList :: UArray Word16 Word32

main = do
    origElt <- select "#orig"
    destElt <- select "#dest"

    setHtml stationOptions origElt
    setHtml stationOptions destElt

    change recalculate def origElt
    change recalculate def destElt

    return ()

recalculate _ = do
    origVal <- select "#orig" >>= getVal
    destVal <- select "#dest" >>= getVal

    let
        orig = read $ unpack origVal :: Station
        dest = read $ unpack destVal :: Station
        -- distance = trackDistance orig dest
        distance = fromIntegral $ distances ! (fromIntegral $ (1 + fromEnum (maxBound :: Station)) * (fromEnum orig) + (fromEnum dest))
        peak = fare Peak distance
        offPeak = fare OffPeak distance
        in do
        select "#distance" >>= setHtml (pack $ show distance)
        select "#peak" >>= setHtml (pack $ show peak)
        select "#off_peak" >>= setHtml (pack $ show offPeak)

    return ()
