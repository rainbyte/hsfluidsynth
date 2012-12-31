module Sound.Fluidsynth where

import Foreign.ForeignPtr.Safe

import Sound.Fluidsynth.Internal

newtype Settings = Settings { unSettings :: ForeignPtr C'fluid_settings_t }

newSettings :: IO Settings
newSettings = do
    ptr <- c'new_fluid_settings
    settings <- newForeignPtr p'delete_fluid_settings ptr
    return $! Settings settings
