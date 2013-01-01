module Main where

import System.Environment

import Sound.Fluidsynth

main = do
    soundfont:midi:_ <- getArgs
    settings <- newSettings
    synth <- newSynth settings
    loadSF synth soundfont
    driver <- newDriver settings synth
    player <- newPlayer synth
    playerAdd player midi
    playerPlay player
    playerJoin player
