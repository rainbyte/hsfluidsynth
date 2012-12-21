{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Sound.Fluidsynth.Internal.Player where

import Foreign.C
import Foreign.ForeignPtr.Safe
import Foreign.Ptr

import Sound.Fluidsynth.Internal.Synth
import Sound.Fluidsynth.Internal.Type

data Player

type PlayerPtr = ForeignPtr Player

foreign import ccall "new_fluid_player" newPlayer
    :: Ptr Synth -> IO (Ptr Player)

foreign import ccall "&delete_fluid_player" deletePlayer
    :: FunPtr (Ptr Player -> IO ())

makePlayer :: SynthPtr -> FS PlayerPtr
makePlayer synth = FS $ withForeignPtr synth $ \ptr -> do
    p <- newPlayer ptr
    newForeignPtr deletePlayer p
