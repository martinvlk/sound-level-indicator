module Main where

import Data.Time.Calendar           ( Day(..) )
import Data.Time.Clock              ( getCurrentTime
                                    , UTCTime(..)
                                    )

import Control.Concurrent           ( forkIO
                                    , ThreadId
                                    , killThread
                                    , threadDelay
                                    )
import Control.Concurrent.STM       ( STM
                                    , atomically
                                    )
import Control.Concurrent.STM.TChan ( TChan
                                    , newTChanIO
                                    , tryReadTChan
                                    , writeTChan
                                    )
import Control.Monad                ( void
                                    , forever
                                    )
import Control.Monad.IO.Class       ( liftIO )
import Control.Monad.Loops          ( iterateUntilM
                                    , whileJust
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

import UI.NCurses as NC             ( runCurses
                                    , Window
                                    , defaultWindow
                                    , setEcho
                                    , getEvent
                                    , Event(..)
                                    , updateWindow
                                    , moveCursor
                                    , drawString
                                    , render
                                    , Curses
                                    , clear
                                    , screenSize
                                    )

--------------------------------------------------------------------------------

data World = World { inputs :: WInputs
                   , win :: NC.Window
                   , ballPosition :: Double
                   , soundData :: SoundData
                   , stop :: Bool
                   , lastFrameTime :: Double
                   , gameDeltaTime :: Double
                   , realDeltaTime :: Double
                   , soundTime :: Double }

data WInputs = WInputs { pulseSource :: Simple
                       , soundChan :: TChan SoundData
                       , sndThreadId :: ThreadId }

data Inputs = In { samples :: SoundData
                 , escPressed :: Bool }
type SoundData = [Double]
type AvgAmplitude = Double

--------------------------------------------------------------------------------

main :: IO ()
main =
  runCurses $
  initWorld >>= iterateUntilM stop mainLoop >>= liftIO . destroyWorld

targetFrameTime :: Double
targetFrameTime = 1000 / 60 -- run at 60fps

gameTimeFactor :: Double
gameTimeFactor = 1 -- run at 1:1 time

amplificationFactor :: Double
amplificationFactor = 1000

soundProcesInterval :: Double
soundProcesInterval = 0.250 -- every 250 ms

mainLoop :: World -> Curses World
mainLoop = withGameTime (\w -> do
                             i <- processInputs (inputs w) (win w)
                             let w' = updateWorld w i
                             generateOutputs w'
                             return w' )

withGameTime :: (World -> Curses World) -> World -> Curses World
withGameTime frameWorkload w = do
  timeStart <- liftIO $ toSeconds =<< getCurrentTime
  let realDt = timeStart - lastFrameTime w -- time since last frame
      gameDt = realDt * gameTimeFactor -- apply slow-down or speed-up

  w' <- frameWorkload $ w { gameDeltaTime = gameDt
                          , realDeltaTime = realDt
                          , lastFrameTime = timeStart }

  timeDone <- liftIO $ toSeconds =<< getCurrentTime
  liftIO $ limitFrameRate targetFrameTime $ timeDone - timeStart

  return w'

  where
    toSeconds (UTCTime day dt) = return $
      (fromInteger (toModifiedJulianDay day) * 86400) + realToFrac dt
    limitFrameRate tgt act | act < tgt = threadDelay $
                                         ceiling $ (tgt - act) * 1000 --microsec
                           | act == tgt = return ()
                           | otherwise = error "Exceeded frame time!"

initWorld :: Curses World
initWorld = do
  s <- liftIO $ simpleNew Nothing "sound-level-indicator" Record Nothing
       "Displaying sound level from default mic."
       (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
  sndChan <- liftIO newTChanIO
  threadId <- liftIO $ forkIO $ forever $ handleMic s sndChan

  setEcho False
  w <- defaultWindow
  updateWindow w clear >> render

  return World {
    inputs = WInputs {
        pulseSource = s
        , soundChan = sndChan
        , sndThreadId = threadId } 
    , win = w
    , ballPosition = 0
    , stop = False
    , lastFrameTime = 0
    , gameDeltaTime = 0
    , realDeltaTime = 0
    , soundData = []
    , soundTime = 0 }

handleMic :: Simple -> TChan SoundData -> IO ()
handleMic s sndChan = do
  sd <- simpleRead s samplesChunkSize
  void . atomically $ writeTChan sndChan sd

  where
    samplesChunkSize = 1000

processInputs :: WInputs -> Window -> Curses Inputs
processInputs i w = do
  ev <- getEvent w timeout
  let isEsc = maybe False (== escChar) ev
  sd <- liftIO $ atomically (readSoundData i)
  return In { samples = sd
            , escPressed = isEsc }
  where
    timeout = Just 2
    escChar = EventCharacter '\ESC'

readSoundData :: WInputs -> STM SoundData
readSoundData i = do
  dta <- whileJust (tryReadTChan $ soundChan i) return
  return $ concat dta

processSoundData :: SoundData -> AvgAmplitude
processSoundData [] = 0
processSoundData s = let d = fmap ((*amplificationFactor) . abs) s
                     in min 1 $ sum d / fromIntegral (length d)

updateWorld :: World -> Inputs -> World
updateWorld w i = let sd = soundData w ++ samples i
                      w' = w { stop = escPressed i }
                  in
                    if soundTime w >= soundProcesInterval
                    then w' { ballPosition = processSoundData sd 
                            , soundData = []
                            , soundTime = 0 }
                    else w' { soundData = sd
                            , soundTime = soundTime w' + realDeltaTime w' }

generateOutputs :: World -> Curses ()
generateOutputs w = do
  (_, cols) <- screenSize
  let indent = 2
      width = fromIntegral $ cols - indent
  updateWindow (win w) $ do
    draw 1 indent width $ soundMeter width
    draw 3 indent width $ "Signal level: " ++ show (ballPosition w)
  render

  where
    draw ln col width content = do
      moveCursor ln col
      drawString (replicate width ' ') -- clear the line
      moveCursor ln col
      drawString content -- draw new content
    soundMeter width = let len = ceiling $ fromIntegral width * ballPosition w
                       in replicate len '*'

destroyWorld :: World -> IO ()
destroyWorld w = let ins = inputs w
                 in killThread (sndThreadId ins) >>
                    simpleFree (pulseSource ins)
