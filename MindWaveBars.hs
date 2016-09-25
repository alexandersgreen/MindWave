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

-- | Read the MindWaveInfo from the IORef, and generate an "image".
--   The Float argument is unused, as we can just access the latest MindWaveInfo
--   for each frame...
bars :: IORef MindWaveInfo -> Float -> IO Picture
bars ref _ = do
 mwi <- readIORef ref
 return $ generateBars mwi

-- | A holding place for the function that generates "images" from the MindWaveInfo.
--   Currently, as a proof of concept, this just produces an Picture containing a 
--   printed printed text String.
generateBars :: MindWaveInfo -> Picture
generateBars mwi = Text $ raw_value mwi
