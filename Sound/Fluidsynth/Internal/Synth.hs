{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Sound.Fluidsynth.Internal.Synth where

import Foreign.C
import Foreign.ForeignPtr.Safe
import Foreign.Ptr

import Sound.Fluidsynth.Internal.Settings
import Sound.Fluidsynth.Internal.Type

data PSynth

type SynthPtr = ForeignPtr PSynth

foreign import ccall "new_fluid_synth" newSynth
    :: Ptr PSettings -> IO (Ptr PSynth)

foreign import ccall "&delete_fluid_synth" deleteSynth
    :: FunPtr (Ptr PSynth -> IO ())

makeSynth :: SettingsPtr -> FS SynthPtr
makeSynth settings = FS $ withForeignPtr settings $ \ptr -> do
    s <- newSynth ptr
    newForeignPtr deleteSynth s
