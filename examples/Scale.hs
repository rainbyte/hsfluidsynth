-- Copyright 2014 Google Inc. All rights reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not
-- use this file except in compliance with the License. You may obtain a copy of
-- the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations under
-- the License.
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
