{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Sound.Fluidsynth.Internal.Event where

import Foreign.C
import Foreign.ForeignPtr.Safe
import Foreign.Ptr

import Sound.Fluidsynth.Internal.Type

data Event

type EventPtr = ForeignPtr Event

foreign import ccall "new_fluid_event" newEvent :: IO (Ptr Event)

foreign import ccall "&delete_fluid_event" deleteEvent
    :: FunPtr (Ptr Event -> IO ())

makeEvent :: FS EventPtr
makeEvent = FS $ newEvent >>= newForeignPtr deleteEvent
