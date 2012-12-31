module Sound.Fluidsynth where

import Foreign.ForeignPtr.Safe

import Sound.Fluidsynth.Internal

newtype Settings = Settings (ForeignPtr C'fluid_settings_t)
newtype Synth = Synth (ForeignPtr C'fluid_synth_t)
newtype Driver = Driver (ForeignPtr C'fluid_audio_driver_t)
newtype Player = Player (ForeignPtr C'fluid_player_t)

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

newDriver :: Settings -> Synth -> IO Driver
newDriver (Settings settings) (Synth synth) = do
    withForeignPtr settings $ \ptr -> do
        withForeignPtr synth $ \ptr' -> do
            ptr'' <- c'new_fluid_audio_driver ptr ptr'
            driver <- newForeignPtr p'delete_fluid_audio_driver ptr''
            return $! Driver driver

newPlayer :: Synth -> IO Player
newPlayer (Synth synth) = do
    withForeignPtr synth $ \ptr -> do
        ptr' <- c'new_fluid_player ptr
        player <- newForeignPtr p'delete_fluid_player ptr'
        return $! Player player
