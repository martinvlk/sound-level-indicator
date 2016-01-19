module MicChan (
      SoundData(..)
    , Mic(..)
    , openMic
    , closeMic
    , readSoundData
  ) where

import Control.Monad                ( void
                                    , forever
                                    )
import Control.Monad.Loops          ( whileJust
                                    )
import Control.Concurrent           ( ThreadId
                                    , forkIO
                                    , killThread
                                    )
import Control.Concurrent.STM       ( atomically
                                    , STM
                                    )
import Control.Concurrent.STM.TChan ( TChan
                                    , newTChanIO
                                    , writeTChan
                                    , tryReadTChan
                                    )
import Sound.Pulse.Simple           ( Simple
                                    , Direction(..)
                                    , SampleSpec(..)
                                    , SampleFormat(..)
                                    , Endian(..)
                                    , simpleNew
                                    , simpleFree
                                    , simpleRead
                                    )

newtype SoundData = SD { samples :: [Double] }
                  deriving Show
data Mic = Mic { chan     :: TChan SoundData
               , threadId :: ThreadId
               , pulseSource :: Simple }

openMic :: IO Mic
openMic = do
  s <- simpleNew Nothing "sound-level-indicator" Record Nothing
       "Displaying sound level from default mic."
       (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
  sndChan <- newTChanIO
  tid <- forkIO $ forever $ handleMic s sndChan
  return $ Mic { chan = sndChan
               , threadId = tid
               , pulseSource = s
               }

closeMic :: Mic -> IO ()
closeMic m = let tid = threadId m
                 s = pulseSource m
             in killThread tid >>
                simpleFree s

handleMic :: Simple -> TChan SoundData -> IO ()
handleMic s sndChan = do
  sd <- simpleRead s samplesChunkSize
  void . atomically $ writeTChan sndChan $ SD sd

  where
    samplesChunkSize = 1000

instance Monoid SoundData where
  mempty = SD []
  mappend (SD d1) (SD d2) = SD $ mappend d1 d2

readSoundData :: Mic -> STM SoundData
readSoundData m = do
  dta <- whileJust (tryReadTChan $ chan m) return
  return $ mconcat dta
