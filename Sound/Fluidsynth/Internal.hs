{-# LANGUAGE EmptyDataDecls #-}

module Sound.Fluidsynth.Internal where

import Foreign.C
import Foreign.Ptr

-- | A restricted monad for FS actions.
newtype FS a = FS (IO a)

instance Functor FS where
    fmap f (FS ma) = FS $ fmap f ma

instance Monad FS where
    (FS ma) >>= f = FS $ ma >>= \x -> case f x of FS mb -> mb
    return = FS . return
    fail = FS . fail

data FluidSettings

foreign import ccall "new_fluid_settings" newFluidSettings
    :: IO (Ptr FluidSettings)
