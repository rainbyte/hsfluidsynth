module Sound.Fluidsynth where

import Foreign.ForeignPtr.Safe

import Sound.Fluidsynth.Internal

newtype Settings = Settings (ForeignPtr C'fluid_settings_t)
newtype Synth = Synth (ForeignPtr C'fluid_synth_t)

newSettings :: IO Settings
newSettings = do
    ptr <- c'new_fluid_settings
    settings <- newForeignPtr p'delete_fluid_settings ptr
    return $! Settings settings

newSynth :: Settings -> IO Synth
newSynth (Settings settings) = do
    withForeignPtr settings $ \ptr -> do
        ptr' <- c'new_fluid_synth ptr
        synth <- newForeignPtr p'delete_fluid_synth ptr'
        return $! Synth synth
