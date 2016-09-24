module Main where

import Data.IORef
import Control.Concurrent
import Control.Exception
import MindWaveConnection
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate

-- | An example usage of the MindWaveConnection module, that creates an IORef, 
--   forks off a thread for reading from the MindWave, and then uses this thread to
--   generate "images" representing the continaully updating MindWaveInfo 
main :: IO ()
main = do
 ref <- newIORef initialMindWaveInfo
 mindWaveThread <- forkIO $ readMind ref
 finally (animateFixedIO (InWindow "MindWaveBars" (1024,768) (10,10)) white (bars ref)) $ do
  killThread mindWaveThread
  disconnect
  print "End of Program"

bars :: IORef MindWaveInfo -> Float -> IO Picture
bars ref _ = do
 mwi <- readIORef ref
 return $ generateBars mwi

generateBars :: MindWaveInfo -> Picture
generateBars mwi = Text $ raw_value mwi
