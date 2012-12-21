{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Sound.Fluidsynth.Internal.Settings where

import Foreign.C
import Foreign.ForeignPtr.Safe
import Foreign.Ptr

import Sound.Fluidsynth.Internal.Type

data Settings

type SettingsPtr = ForeignPtr Settings

data SettingsType = STNone | STNum | STInt | STStr | STBool
    deriving (Eq, Show)

foreign import ccall "new_fluid_settings" newSettings :: IO (Ptr Settings)

foreign import ccall "&delete_fluid_settings" deleteSettings
    :: FunPtr (Ptr Settings -> IO ())

foreign import ccall "fluid_settings_get_type" settingsGetType
    :: Ptr Settings -> CString -> IO (CInt)

makeSettings :: FS SettingsPtr
makeSettings = FS $ do
    s <- newSettings
    newForeignPtr deleteSettings s

settingsGetType' :: SettingsPtr -> String -> FS SettingsType
settingsGetType' settings key = FS $ withCString key f
    where
        f cstring = do
            s <- withForeignPtr settings $ \ptr ->
                settingsGetType ptr cstring
            return $ case s of
                0 -> STNum
                1 -> STInt
                2 -> STStr
                3 -> STBool
                _ -> STNone
