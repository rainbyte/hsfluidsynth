{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Sound.Fluidsynth.Internal.Sequencer where

import Foreign.C
import Foreign.ForeignPtr.Safe
import Foreign.Ptr

import Sound.Fluidsynth.Internal.Type

data PSequencer

type SequencerPtr = ForeignPtr PSequencer

foreign import ccall "new_fluid_event" newSequencer :: IO (Ptr PSequencer)

foreign import ccall "&delete_fluid_event" deleteSequencer
    :: FunPtr (Ptr PSequencer -> IO ())

makeSequencer :: FS SequencerPtr
makeSequencer = FS $ newSequencer >>= newForeignPtr deleteSequencer
