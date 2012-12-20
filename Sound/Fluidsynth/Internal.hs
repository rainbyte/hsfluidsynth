{-# LANGUAGE EmptyDataDecls #-}

module Sound.Fluidsynth.Internal where

import Foreign.C
import Foreign.ForeignPtr.Safe
import Foreign.Ptr

-- | A restricted monad for FS actions.
newtype FS a = FS (IO a)

instance Functor FS where
    fmap f (FS ma) = FS $ fmap f ma

instance Monad FS where
    (FS ma) >>= f = FS $ ma >>= \x -> case f x of FS mb -> mb
    return = FS . return
    fail = FS . fail

data Settings

data SettingsType = STNone | STNum | STInt | STStr | STBool
    deriving (Eq, Show)

foreign import ccall "new_fluid_settings" newSettings :: IO (Ptr Settings)

foreign import ccall "&delete_fluid_settings" deleteSettings
    :: FunPtr (Ptr Settings -> IO ())

foreign import ccall "fluid_settings_get_type" settingsGetType
    :: Ptr Settings -> CString -> IO (CInt)

makeSettings :: FS (ForeignPtr Settings)
makeSettings = FS $ do
    s <- newSettings
    newForeignPtr deleteSettings s

settingsGetType' :: Ptr Settings -> String -> FS SettingsType
settingsGetType' settings key = FS $ withCString key f
    where
        f cstring = do
            s <- settingsGetType settings cstring
            return $ case s of
                0 -> STNum
                1 -> STInt
                2 -> STStr
                3 -> STBool
                _ -> STNone
