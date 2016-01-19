module Main where

import Data.Time.Calendar           ( Day(..) )
import Data.Time.Clock              ( getCurrentTime
                                    , UTCTime(..)
                                    )

import Control.Concurrent           ( threadDelay
                                    )
import Control.Concurrent.STM       ( STM
                                    , atomically
                                    )
import Control.Concurrent.STM.TChan ( tryReadTChan
                                    )
import Control.Monad.IO.Class       ( liftIO )
import Control.Monad.Loops          ( iterateUntilM
                                    , whileJust
                                    )

import qualified UI.NCurses as NC   ( runCurses
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

import qualified MicChan as MC

--------------------------------------------------------------------------------

data World = World { inputs :: WInputs
                   , win :: NC.Window
                   , ballPosition :: Double
                   , soundData :: MC.SoundData
                   , stop :: Bool
                   , lastFrameTime :: Double
                   , gameDeltaTime :: Double
                   , realDeltaTime :: Double
                   , soundTime :: Double }

data WInputs = WInputs { mic :: MC.Mic }

data Inputs = In { micData :: MC.SoundData
                 , escPressed :: Bool }
type AvgAmplitude = Double

--------------------------------------------------------------------------------

main :: IO ()
main =
  NC.runCurses $
  initWorld >>= iterateUntilM stop mainLoop >>= liftIO . destroyWorld

targetFrameTime :: Double
targetFrameTime = 1000 / 60 -- run at 60fps

gameTimeFactor :: Double
gameTimeFactor = 1 -- run at 1:1 time

amplificationFactor :: Double
amplificationFactor = 1000

soundProcesInterval :: Double
soundProcesInterval = 0.250 -- every 250 ms

mainLoop :: World -> NC.Curses World
mainLoop = withGameTime (\w -> do
                             i <- processInputs (inputs w) (win w)
                             let w' = updateWorld w i
                             generateOutputs w'
                             return w' )

withGameTime :: (World -> NC.Curses World) -> World -> NC.Curses World
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

initWorld :: NC.Curses World
initWorld = do
  m <- liftIO MC.openMic

  NC.setEcho False
  w <- NC.defaultWindow
  NC.updateWindow w NC.clear >> NC.render

  return World {
    inputs = WInputs { mic = m } 
    , win = w
    , ballPosition = 0
    , stop = False
    , lastFrameTime = 0
    , gameDeltaTime = 0
    , realDeltaTime = 0
    , soundData = mempty
    , soundTime = 0 }

processInputs :: WInputs -> NC.Window -> NC.Curses Inputs
processInputs i w = do
  ev <- NC.getEvent w timeout
  let isEsc = maybe False (== escChar) ev
  sd <- liftIO $ atomically (MC.readSoundData $ mic i)
  return In { micData = sd
            , escPressed = isEsc }
  where
    timeout = Just 2
    escChar = NC.EventCharacter '\ESC'

processSoundData :: MC.SoundData -> AvgAmplitude
processSoundData (MC.SD []) = 0
processSoundData (MC.SD s) = let d = fmap ((*amplificationFactor) . abs) s
                             in min 1 $ sum d / fromIntegral (length d)

updateWorld :: World -> Inputs -> World
updateWorld w i = let sd = soundData w `mappend` micData i
                      w' = w { stop = escPressed i }
                  in
                    if soundTime w >= soundProcesInterval
                    then w' { ballPosition = processSoundData sd 
                            , soundData = mempty
                            , soundTime = 0 }
                    else w' { soundData = sd
                            , soundTime = soundTime w' + realDeltaTime w' }

generateOutputs :: World -> NC.Curses ()
generateOutputs w = do
  (_, cols) <- NC.screenSize
  let indent = 2
      width = fromIntegral $ cols - indent
  NC.updateWindow (win w) $ do
    draw 1 indent width $ soundMeter width
    draw 3 indent width $ "Signal level: " ++ show (ballPosition w)
  NC.render

  where
    draw ln col width content = do
      NC.moveCursor ln col
      NC.drawString (replicate width ' ') -- clear the line
      NC.moveCursor ln col
      NC.drawString content -- draw new content
    soundMeter width = let len = ceiling $ fromIntegral width * ballPosition w
                       in replicate len '*'

destroyWorld :: World -> IO ()
destroyWorld w = MC.closeMic (mic $ inputs w)
