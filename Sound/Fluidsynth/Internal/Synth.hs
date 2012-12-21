{-# LANGUAGE EmptyDataDecls #-}

module Sound.Fluidsynth.Internal.Synth where

import Foreign.C
import Foreign.ForeignPtr.Safe
import Foreign.Ptr

import Sound.Fluidsynth.Internal.Settings
import Sound.Fluidsynth.Internal.Type

data Synth

type SynthPtr = ForeignPtr Synth

foreign import ccall "new_fluid_synth" newSynth
    :: Ptr Settings -> IO (Ptr Synth)

foreign import ccall "&delete_fluid_snyth" deleteSynth
    :: FunPtr (Ptr Synth -> IO ())

makeSynth :: SettingsPtr -> FS SynthPtr
makeSynth settings = FS $ do
    withForeignPtr settings $ \ptr -> do
        s <- newSynth ptr
        newForeignPtr deleteSynth s
