-- file src/Main.hs
-- Bringing it all together.

import Brick hiding (on)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, void)
import Data.Function (on)
import Data.List (sortBy)
import Graphics.Vty (mkVty, defaultConfig)
import Lens.Micro (_3)
import Lens.Micro.Extras (view)
import System.Environment (getEnv)

import Greed (score)
import Scores
import UI

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Progression
    threadDelay 100000
  g <- initialState
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty (Just chan) app g  

  homeDir <- getEnv "HOME"

  let scoreFile = homeDir ++ "/.hgreedscores"

  existingScores <- readScores scoreFile
  newScore <- composeScore finalState
  
  putStrLn $ showScore finalState

  case existingScores of
    Nothing -> do
      putStrLn "No existing high scores"
      writeScores scoreFile [newScore]
    Just es -> do
      putStrLn "High scores:"
      putStrLn $ prettyScores $
                 take 10 $
                 reverse $
                 sortBy (compare `on` (\x -> view _3 x)) es
      writeScores scoreFile (newScore:es) 
