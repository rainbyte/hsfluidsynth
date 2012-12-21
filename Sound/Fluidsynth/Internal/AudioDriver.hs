{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Sound.Fluidsynth.Internal.AudioDriver where

import Foreign.C
import Foreign.ForeignPtr.Safe
import Foreign.Ptr

import Sound.Fluidsynth.Internal.Settings
import Sound.Fluidsynth.Internal.Synth
import Sound.Fluidsynth.Internal.Type

data AudioDriver

type AudioDriverPtr = ForeignPtr AudioDriver

foreign import ccall "new_fluid_audio_driver" newAudioDriver
    :: Ptr Settings -> Ptr Synth -> IO (Ptr AudioDriver)

foreign import ccall "&delete_fluid_audio_driver" deleteAudioDriver
    :: FunPtr (Ptr AudioDriver -> IO ())

makeAudioDriver :: SettingsPtr -> SynthPtr -> FS AudioDriverPtr
makeAudioDriver settings synth = FS $ do
    withForeignPtr settings $ \ptr -> do
        withForeignPtr synth $ \ptr' -> do
            ad <- newAudioDriver ptr ptr'
            newForeignPtr deleteAudioDriver ad
