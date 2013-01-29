module Main where

import Control.Concurrent
import Control.Monad
import System.Environment

import Sound.Fluidsynth

scale = [60, 62, 64, 65, 67, 69, 71, 72]

main = do
    sf:_ <- getArgs
    settings <- newSettings
    synth <- newSynth settings
    driver <- newDriver synth
    loadSF synth sf
    forM scale $ \note -> do
        synthNoteOn synth 0 note 127
        threadDelay $ 500 * 1000
        synthNoteOff synth 0 note
    forM (reverse scale) $ \note -> do
        synthNoteOn synth 0 note 127
        threadDelay $ 500 * 1000
        synthNoteOff synth 0 note
