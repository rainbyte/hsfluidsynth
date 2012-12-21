{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Sound.Fluidsynth.Internal.AudioDriver where

import Foreign.C
import Foreign.ForeignPtr.Safe
import Foreign.Ptr

import Sound.Fluidsynth.Internal.Settings
import Sound.Fluidsynth.Internal.Synth
import Sound.Fluidsynth.Internal.Type

data PAudioDriver

type AudioDriverPtr = ForeignPtr PAudioDriver

foreign import ccall "new_fluid_audio_driver" newAudioDriver
    :: Ptr PSettings -> Ptr PSynth -> IO (Ptr PAudioDriver)

foreign import ccall "&delete_fluid_audio_driver" deleteAudioDriver
    :: FunPtr (Ptr PAudioDriver -> IO ())

makeAudioDriver :: SettingsPtr -> SynthPtr -> FS AudioDriverPtr
makeAudioDriver settings synth = FS $ withForeignPtr settings $ \ptr ->
    withForeignPtr synth $ \ptr' -> do
        ad <- newAudioDriver ptr ptr'
        newForeignPtr deleteAudioDriver ad
