module Main where

import Control.Concurrent
import Control.Monad
import System.Environment

import Sound.Fluidsynth

notes  = [79, 78, 79, 74, 79, 69, 79, 67, 79, 72, 79, 76,
         79, 78, 79, 74, 79, 69, 79, 67, 79, 72, 79, 76,
         79, 78, 79, 74, 79, 72, 79, 76, 79, 78, 79, 74,
         79, 72, 79, 76, 79, 78, 79, 74, 79, 72, 79, 76,
         79, 76, 74, 71, 69, 67, 69, 67, 64, 67, 64, 62,
         64, 62, 59, 62, 59, 57, 64, 62, 59, 62, 59, 57,
         64, 62, 59, 62, 59, 57, 43]

main = do
    sf:_ <- getArgs
    settings <- newSettings
    synth <- newSynth settings
    driver <- newDriver settings synth
    loadSF synth sf
    forM notes $ \note -> do
        synthNoteOn synth 0 note 127
        threadDelay $ 100 * 1000
        synthNoteOff synth 0 note
