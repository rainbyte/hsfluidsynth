{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sound.Fluidsynth
    (newSettings
    ,newSynth
    ,newDriver
    ,newPlayer
    ,loadSF
    ,playerAdd
    ,playerPlay
    ,playerJoin
    ,synthNoteOn
    ,synthNoteOff
    ,eventNoteOn
    ,eventNoteOff)
where

import Control.Monad
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr.Safe
import Foreign.Ptr

import Sound.Fluidsynth.Internal

newtype Settings = Settings (ForeignPtr C'fluid_settings_t)
newtype Synth = Synth (ForeignPtr C'fluid_synth_t)
newtype Driver = Driver (ForeignPtr C'fluid_audio_driver_t)
newtype Player = Player (ForeignPtr C'fluid_player_t)
newtype Event = Event (ForeignPtr C'fluid_event_t)

newtype Channel = Channel Int
    deriving (Enum, Eq, Integral, Ord, Num, Real)
newtype Key = Key Int
    deriving (Enum, Eq, Integral, Ord, Num, Real)
newtype Velocity = Velocity Int
    deriving (Enum, Eq, Integral, Ord, Num, Real)
newtype Program = Program Int
    deriving (Enum, Eq, Integral, Ord, Num, Real)

newSettings :: IO Settings
newSettings = do
    ptr <- c'new_fluid_settings
    settings <- newForeignPtr p'delete_fluid_settings ptr
    withForeignPtr settings $ \ptr' ->
        withCAString "audio.driver" $ \cstr ->
            withCAString "alsa" $ \cstr' ->
                void $ c'fluid_settings_setstr ptr' cstr cstr'
    return $! Settings settings

newSynth :: Settings -> IO Synth
newSynth (Settings settings) = do
    withForeignPtr settings $ \ptr -> do
        ptr' <- c'new_fluid_synth ptr
        synth <- newForeignPtr p'delete_fluid_synth ptr'
        return $! Synth synth

newDriver :: Settings -> Synth -> IO Driver
newDriver (Settings settings) (Synth synth) = do
    withForeignPtr settings $ \ptr -> do
        withForeignPtr synth $ \ptr' -> do
            ptr'' <- c'new_fluid_audio_driver ptr ptr'
            driver <- newForeignPtr p'delete_fluid_audio_driver ptr''
            return $! Driver driver

newPlayer :: Synth -> IO Player
newPlayer (Synth synth) = do
    withForeignPtr synth $ \ptr -> do
        ptr' <- c'new_fluid_player ptr
        player <- newForeignPtr p'delete_fluid_player ptr'
        return $! Player player

loadSF :: Synth -> String -> IO ()
loadSF (Synth synth) path = do
    withForeignPtr synth $ \ptr ->
        withCAString path $ \cstr ->
            void $ c'fluid_synth_sfload ptr cstr 1

synthNoteOn :: Synth -> Channel -> Key -> Velocity -> IO ()
synthNoteOn (Synth synth) c k v =
    void $ withForeignPtr synth $ \ptr ->
        c'fluid_synth_noteon ptr (fromIntegral c) (fromIntegral k)
            (fromIntegral v)

synthNoteOff :: Synth -> Channel -> Key -> IO ()
synthNoteOff (Synth synth) c k =
    withForeignPtr synth $ \ptr ->
        void $ c'fluid_synth_noteoff ptr (fromIntegral c) (fromIntegral k)

playerAdd :: Player -> String -> IO ()
playerAdd (Player player) path = do
    withForeignPtr player $ \ptr ->
        withCAString path $ \cstr ->
            void $ c'fluid_player_add ptr cstr

playerPlay :: Player -> IO ()
playerPlay (Player player) = do
    withForeignPtr player c'fluid_player_play
    return ()

playerJoin :: Player -> IO ()
playerJoin (Player player) = do
    withForeignPtr player c'fluid_player_join
    return ()

-- | Make an event.
--
--   Since the event is unpatterned, it isn't going to be very useful. End
--   users almost certainly want the patterned event creators.
newEvent :: IO Event
newEvent = do
    ptr <- c'new_fluid_event
    event <- newForeignPtr p'delete_fluid_event ptr
    return $! Event event

-- | Make an event and call an action on it.
--
--   Just a combinator meant to help write the following bindings.
withNewEvent :: (Ptr C'fluid_event_t -> IO ()) -> IO Event
withNewEvent action = do
    e@(Event event) <- newEvent
    withForeignPtr event action
    return e

eventNoteOn :: Channel -> Key -> Velocity -> IO Event
eventNoteOn c k v = withNewEvent $ \ptr ->
    c'fluid_event_noteon ptr (fromIntegral c) (fromIntegral k)
        (fromIntegral v)

eventNoteOff :: Channel -> Key -> IO Event
eventNoteOff c k = withNewEvent $ \ptr ->
    c'fluid_event_noteoff ptr (fromIntegral c) (fromIntegral k)

eventPitchSens :: Channel -> Int -> IO Event
eventPitchSens c amount = withNewEvent $ \ptr ->
    c'fluid_event_pitch_wheelsens ptr (fromIntegral c) (fromIntegral amount)

eventPitchBend :: Channel -> Int -> IO Event
eventPitchBend c amount = withNewEvent $ \ptr ->
    c'fluid_event_pitch_bend ptr (fromIntegral c) (fromIntegral amount)

eventProgramControl :: Channel -> Program -> IO Event
eventProgramControl c p = withNewEvent $ \ptr ->
    c'fluid_event_program_change ptr (fromIntegral c) (fromIntegral p)

eventVolume :: Channel -> Int -> IO Event
eventVolume c amount = withNewEvent $ \ptr ->
    c'fluid_event_volume ptr (fromIntegral c) (fromIntegral amount)
