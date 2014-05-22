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
    driver <- newDriver synth
    loadSF synth sf
    forM notes $ \note -> do
        synthNoteOn synth 0 note 127
        threadDelay $ 100 * 1000
        synthNoteOff synth 0 note
    threadDelay $ 200 * 1000
