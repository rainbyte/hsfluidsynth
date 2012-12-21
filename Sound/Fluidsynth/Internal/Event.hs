{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Sound.Fluidsynth.Internal.Event where

import Foreign.C
import Foreign.ForeignPtr.Safe
import Foreign.Ptr

import Sound.Fluidsynth.Internal.Type

data PEvent

type EventPtr = ForeignPtr PEvent

foreign import ccall "new_fluid_event" newEvent :: IO (Ptr PEvent)

foreign import ccall "&delete_fluid_event" deleteEvent
    :: FunPtr (Ptr PEvent -> IO ())

makeEvent :: FS EventPtr
makeEvent = FS $ newEvent >>= newForeignPtr deleteEvent
