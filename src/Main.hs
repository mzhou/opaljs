{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Data.Default (def)
import Data.Text (Text, pack, unpack)
import JavaScript.JQuery
import Language.Haskell.TH.Syntax (lift)

import Opal.Distance
import Opal.Fare.Train

stationOptions = $(lift $ foldr (++) "" (map (\x -> "<option value=\"" ++ x ++ "\">" ++ x ++ "</option>") (map show stations)))

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
        orig = read $ unpack origVal
        dest = read $ unpack destVal
        distance = trackDistance orig dest
        peak = fare Peak distance
        offPeak = fare OffPeak distance
        in do
        select "#distance" >>= setHtml (pack $ show distance)
        select "#peak" >>= setHtml (pack $ show peak)
        select "#off_peak" >>= setHtml (pack $ show offPeak)

    return ()
