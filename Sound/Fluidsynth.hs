module Sound.Fluidsynth
    (newSettings
    ,newSynth
    ,newDriver
    ,newPlayer
    ,loadSF
    ,playerAdd
    ,playerPlay
    ,playerJoin)
where

import Control.Monad
import Foreign.C.String
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

loadSF :: Synth -> String -> IO ()
loadSF (Synth synth) path = do
    withForeignPtr synth $ \ptr ->
        withCAString path $ \cstr ->
            void $ c'fluid_synth_sfload ptr cstr 1

playerAdd :: Player -> String -> IO ()
playerAdd (Player player) path = do
    withForeignPtr player $ \ptr ->
        withCAString path $ \cstr ->
            void $ c'fluid_player_add ptr cstr

playerPlay :: Player -> IO ()
playerPlay (Player player) = do
    withForeignPtr player c'fluid_player_play
    return ()

playerJoin :: Player -> IO ()
playerJoin (Player player) = do
    withForeignPtr player c'fluid_player_join
    return ()
