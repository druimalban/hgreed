-- file src/Scores.hs
-- High-scores reading, writing, and parsing.
-- This should be imported by other modules such as UI.

module Scores where

import Brick
import Data.Array (bounds)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Time.LocalTime (getZonedTime)
import Data.Tuple.Sequence (sequenceT)
import Lens.Micro (_1, _2, _3, over)
import Lens.Micro.Extras (view)
import System.Environment (getEnv)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Greed

type TimeStamp = String
type Percentage = Float
type GreedScore = (String, TimeStamp, Score)

scoring :: GreedState -> Widget GreedStateName
scoring = str . showScore

showScore :: GreedState -> String
showScore gs = do
  let sc = calcScore gs
  let ssc = show $ fst sc
  let percentage = printf "%.2f" (snd sc) :: String
  "Score: " ++ ssc ++ "  ~" ++ percentage ++ "%"

calcScore :: GreedState -> (Int, Float)
calcScore gs = do
  let sc  = view score gs
  let sc' = toEnum sc :: Float
  let (_,(c,r)) = bounds $ view gridKey gs
  let max = toEnum ((r+1) * (c+1)) :: Float
  (sc, (sc' / max) * 100)

composeScore :: GreedState -> IO (GreedScore)
composeScore gs = do
  let sc = view score gs
  username <- getEnv "USER"
  timestamp <- getZonedTime
  return (username, show timestamp, sc)

readScores :: FilePath -> IO (Maybe [GreedScore])
readScores fp = do
  ls <- fmap lines (readFile fp)
  let records = map extractScore ls where
        extractScore :: String -> Maybe GreedScore
        extractScore line = do
          let wds = splitOn "," line
          if (length wds) /= 3 then Nothing else
            sequenceT (Just $ head wds,
                       Just $ wds !! 1,
                       readMaybe (last wds) :: Maybe Int)
  let sortedRecords = sequence $ sort records
  return sortedRecords

writeScores :: FilePath -> [GreedScore] -> IO ()
writeScores fp = (writeFile fp . unlines . map prettyC) where
  prettyC :: GreedScore -> String
  prettyC sc = (view _1 sc) ++ "," ++ (view _2 sc) ++ "," ++ (show $ view _3 sc)  

prettyScores :: [GreedScore] -> String
prettyScores = unlines . map pretty where
  pretty :: GreedScore -> String
  pretty sc = (view _1 sc) ++ " " ++ (view _2 sc) ++ " " ++ (show $ view _3 sc)
