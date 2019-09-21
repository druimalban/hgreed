-- file src/Greed.hs
-- Main game logic etc.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Greed where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Array
import Data.Functor.Foldable
import Data.Maybe (fromJust, isJust)
import Data.Tuple.Sequence (sequenceT)
import Lens.Micro (over, set, _1, _2)
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)
import System.Random

data Rose = N | S | E | W | NE | NW | SE | SW deriving (Show, Eq)
type Score = Int
type Percentage = Float
type Pos = (Int, Int)

data Tile = Tile { _tileState :: TileState
                 , _annotation :: Int
                 } deriving (Show, Eq)

data TileState = Eaten | Merely | Marked | Highlighted deriving (Show, Eq)

data Plan = Plan { _dir :: Rose
                 , _elements :: [Pos]
                 } deriving (Show, Eq)

type GridKey = Array Pos Tile

data GreedState = GreedState {
    _gridKey    :: GridKey
  , _position   :: Pos
  , _score      :: Score
  , _showPlans  :: Bool
  , _greedState :: GreedStateName
  } deriving (Show, Eq)

data GreedStateName = Greediness
                    | Blocking
                    | SeekingHelp
                    | Terminus
                    | Quitting
                    | Finale
                    deriving (Show, Eq, Ord)

makeLenses ''Tile
makeLenses ''Plan
makeLenses ''GreedState

nextPosition :: Pos -> Rose -> Pos
nextPosition (x,y) = \case
  N  -> (x, y+1)
  E  -> (x+1, y)
  S  -> (x, y-1)
  W  -> (x-1, y)
  NE -> (x+1, y+1)
  NW -> (x-1, y+1)
  SE -> (x+1, y-1)
  SW -> (x-1, y-1) 

planPath :: Pos -> Rose -> Int -> Plan
planPath pos dir count = Plan dir $ apo coalg (pos, dir, count) where
  coalg (p, d, c)
    | c < 0 = Nil
    | otherwise = Cons p (return $ np p d c) where
        np :: Pos -> Rose -> Int -> (Pos, Rose, Int)
        np p' d' c' = (nextPosition p' d', d', c'-1)

tileWithinGrid :: GreedState -> Pos -> Bool
tileWithinGrid gs p = p `elem` indices (view gridKey gs)

allNeighbours :: GreedState -> [(Rose, Pos)]
allNeighbours gs = filter (validT gs) [(NW, (c-1, r+1)), (N, (c, r+1)), (NE, (c+1, r+1)),
                                        (W,  (c-1, r)),                  (E,  (c+1, r)),
                                        (SW, (c-1, r-1)), (S, (c, r-1)), (SE, (c+1, r-1))]
  where (c, r) = view position gs
        validT :: GreedState -> (Rose, Pos) -> Bool
        validT gs (d, p) = tileWithinGrid gs p
  
pathWithinBounds :: GreedState -> Plan -> Bool
pathWithinBounds gs path = all (tileWithinGrid gs) (view elements path)

legalPath :: GreedState -> Plan -> Bool
legalPath gs path
  | not $ pathWithinBounds gs path = False
  | otherwise = all (not . eaten . (!) (view gridKey gs)) (view elements path)

getTileAt :: GreedState -> Pos -> Maybe Tile
getTileAt gs pos
  | tileWithinGrid gs pos = return (view gridKey gs ! pos)
  | otherwise = Nothing

splitAtCols :: Int -> [a] -> [[a]]
splitAtCols n xs = apo coalg (n, xs) where
  coalg (_, []) = Nil
  coalg (n, xs) = Cons x (return (n, xs')) where
    (x, xs') = splitAt n xs

neighbourInDir :: GreedState -> Rose -> Bool
neighbourInDir gs dir = case lookup dir (allNeighbours gs) of
  Nothing -> False
  Just p -> True

collapsePaths :: [Plan] -> [Pos]
collapsePaths = foldr (\i j -> view elements i ++ j) []

eaten :: Tile -> Bool
eaten t = view tileState t == Eaten

eat :: Tile -> Tile
eat = set tileState Eaten

highlight :: Tile -> Tile
highlight = over tileState tth where
  tth Merely = Highlighted
  tth n = n

cover :: Tile -> Tile
cover = over tileState tc where
  tc Highlighted = Merely
  tc n = n

eatPlans, highlightPlans :: [Plan] -> GreedState -> GreedState
eatPlans       = plansOverState eat
highlightPlans = plansOverState highlight

coverGrid :: GreedState -> GreedState
coverGrid = over (gridKey . traverse) cover

plansOverState :: (Tile -> Tile) -> [Plan] -> GreedState -> GreedState
plansOverState pf paths = over gridKey (hlp paths pf) where
  hlp :: [Plan] -> (Tile -> Tile) -> GridKey -> GridKey
  hlp ps pf gk = gk // map (setup gk pf) (collapsePaths ps)

  setup :: GridKey -> (Tile -> Tile) -> Pos -> (Pos, Tile)
  setup gk pf = id &&& (pf . (gk !))

posOverState :: (Tile -> Tile) -> Pos -> GreedState -> GreedState
posOverState pf p = plansOverState pf [Plan N [p]]
  
buildPlans :: GreedState -> [Plan]
buildPlans gs = do
  let pos = view position gs
  let nbs = allNeighbours gs
  let paths = map (plan gs pos) nbs where
        plan :: GreedState -> Pos -> (Rose, Pos) -> Maybe Plan
        plan gs p nb = do
          let dir = fst nb
          let nbp = snd nb
          let gk  = view gridKey gs
          let nv  = gk ! nbp
          let ts  = view tileState nv
          let n   = view annotation nv
          case ts of
            Eaten       -> Nothing
            Marked      -> Nothing
            _ -> Just $ planPath p dir n
  let projections = fromJust $ sequence $ filter isJust paths
  let finalPlans = filter (legalPath gs) projections
  finalPlans

pathInDir :: GreedState -> Rose -> Maybe Plan
pathInDir gs dir = do 
  let gp (Plan d es) = (d, es)
  nodes <- lookup dir $ map gp $ buildPlans gs
  return $ Plan dir nodes

increment :: Rose -> GreedState -> GreedState
increment dir gs = case view greedState gs of
  Terminus    -> set greedState Finale gs
  SeekingHelp -> set greedState Greediness gs
  _ -> case pathInDir gs dir of
    Nothing ->
      if null (buildPlans gs) then
        set greedState Terminus gs
      else
        set greedState Blocking gs
                      
    Just path -> do
      let lastPos   = view position gs
      let lastScore = view score gs
      let nextPos   = last $ view elements path

      let eatingState      = set greedState Greediness gs -- reset the state
      let eatPlansState    = eatPlans [path] eatingState
      let noHighlightState = coverGrid eatPlansState
      let noMarkState      = posOverState (set tileState Eaten)  lastPos noHighlightState -- Update tile at old position to not be marked
      let newMarkState     = posOverState (set tileState Marked) nextPos noMarkState 
      let nextPosState     = set position nextPos newMarkState
      let nextScoreState   = set score (lastScore + length (view elements path) - 1) nextPosState

      -- set the highlight again as appropriate
      let nextPaths = buildPlans nextScoreState
      if view showPlans gs then highlightPlans nextPaths nextScoreState else nextScoreState

toggleShowPlans :: GreedState -> GreedState
toggleShowPlans gs
  | view showPlans gs = coverGrid (over showPlans not gs) -- if it is true, we don't show the plans, and we falsify it.
  | otherwise = highlightPlans (buildPlans gs) (over showPlans not gs) -- if it is false, we do show the plans, and we also falsify it.

anyKeyPress :: GreedState -> GreedState
anyKeyPress gs = case view greedState gs of
  Terminus -> set greedState Finale gs    -- press any key after  we run out of moves.
  Quitting -> set greedState Greediness gs    -- we press any key on the quit prompt, don't quit!
  SeekingHelp -> set greedState Greediness gs -- press any key to get out of help state.
  _ -> gs

-- catch-all state transition for pressing 'q'
quit :: GreedState -> GreedState
quit gs = case view greedState gs of
  Greediness -> set greedState Quitting gs
  Blocking -> set greedState Quitting gs
  Quitting -> set greedState Finale gs
  Terminus -> set greedState Finale gs
  SeekingHelp -> set greedState Greediness gs
  _ -> gs

-- catch-all state transition for pressing 'y'.
confirm :: GreedState -> GreedState
confirm gs = case view greedState gs of
  Quitting -> set greedState Finale gs
  Terminus -> set greedState Finale gs
  SeekingHelp -> set greedState Greediness gs
  Greediness -> increment NW gs -- 'y' is also a diagonal key!
  Blocking -> increment NW gs 
  _ -> gs

-- catch-all state transition for pressing '?'
seekHelp :: GreedState -> GreedState
seekHelp gs = case view greedState gs of
  SeekingHelp -> set greedState Greediness gs
  Greediness -> set greedState SeekingHelp gs
  Blocking -> set greedState SeekingHelp gs
  Quitting -> set greedState Finale gs
  Terminus -> set greedState Finale gs
  _ -> gs
