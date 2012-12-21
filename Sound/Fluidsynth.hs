module Sound.Fluidsynth where

import Sound.Fluidsynth.Internal.Player
import Sound.Fluidsynth.Internal.Synth
import Sound.Fluidsynth.Internal.Type

data Synth = Synth { sPtr :: SynthPtr }
    deriving (Show)

data Player = Player { pPaused :: Bool, pPtr :: PlayerPtr }
    deriving (Show)

synthPlayer :: Synth -> FS Player
synthPlayer synth = do
    player <- makePlayer $ sPtr synth
    return $! Player True player
