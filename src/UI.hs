-- file src/UI.hs
-- Basic UI definitions.

module UI where

import Brick
import Brick.Widgets.Border (vBorder, hBorder, border)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core (vLimit, hLimit)
import Brick.Themes (Theme, newTheme, themeToAttrMap)
import Control.Monad (replicateM)
import Data.Array (Array, elems, bounds, listArray)
import Data.Functor.Foldable 
import Graphics.Vty (Attr,
                     defAttr,
                     yellow, red, green, cyan, magenta, white, black,
                     brightYellow, brightRed, brightGreen, brightCyan)
import qualified Graphics.Vty as V
import Lens.Micro (over, set, _1, _2)
import Lens.Micro.Extras (view)
import System.Random (randomRIO)

import Greed
import Scores (scoring)

data Progression = Progression

me = "@"

defaultAttrs :: [(AttrName, Attr)]
defaultAttrs =
  [ (attrName "1", fg yellow)
  , (attrName "2", fg red)
  , (attrName "3", fg green)
  , (attrName "4", fg cyan)
  , (attrName "5", fg magenta)
  , (attrName "6", fg brightYellow)
  , (attrName "7", fg brightRed)
  , (attrName "8", fg brightGreen)
  , (attrName "9", fg brightCyan)
  , (attrName "misc",  white `on` black)
  , (attrName "alt",   black `on` white)
  , (attrName "empty", white `on` black)
  , (attrName "path",  black `on` white) ]

defaultTheme :: Theme
defaultTheme = newTheme (white `on` black) defaultAttrs

defaultMap :: AttrMap
defaultMap = themeToAttrMap defaultTheme

tileWidget :: Tile -> Widget GreedStateName
tileWidget t = case view tileState t of
  Marked      -> withAttr (attrName "path")  (str me)
  Highlighted -> withAttr (attrName "path")  (str $ show $ view annotation t)
  Merely      -> withAttr (attrName n)       (str n) where n = show $ view annotation t
  Eaten       -> withAttr (attrName "empty") (str " ")

gridWidget :: GreedState -> Widget GreedStateName
gridWidget gs = case view greedState gs of 
                  SeekingHelp -> border $ padAll 2 $ center helpDialogue
                  _ -> do 
                    let cols = (snd . snd . bounds . view gridKey) gs
                    let el = map reverse $ splitAtCols (cols+1) $ elems $ view gridKey gs
                    cata alg el where
                      alg Nil = emptyWidget
                      alg (Cons x y) = gridLine x <+> y 

                      gridLine :: [Tile] -> Widget GreedStateName
                      gridLine = cata alg'

                      alg' Nil = emptyWidget
                      alg' (Cons x y) = tileWidget x <=> y

dialogue :: GreedState -> Widget GreedStateName
dialogue gs = scoreFmt gs <+> case view greedState gs of 
  Blocking -> hCenter (str "Bad move.")
  Quitting -> hCenter (str "Really quit?")
  Terminus -> hCenter (str "Press any key to quit")
  _ -> hCenter (str "hGreed v0.1 - press '?' for help")
  where
    scoreFmt gs = withAttr (attrName "misc") (scoring gs)

helpDialogue :: Widget GreedStateName
helpDialogue = str "Welcome to hGreed. (Original game by Matthew Day <mday@iconsys.uu.net>.)"
     <=> strWrap ( mconcat
     [ "The object of Greed is to erase as much of the screen as"
     , " possible by moving around in a grid of numbers. To move,"
     , " use the arrow keys, your number pad, or one of the letters"
     , " 'hjklyubn'. Your location is signified by the '%c' symbol."
     , " When you move in a direction, you erase N number of grid"
     , " squares in that direction, N being the first number in that"
     , " direction. Your score reflects the total number of squares"
     , " eaten. Greed will not let you make a move that would have"
     , " placed you off the grid or over a previously eaten square"
     , " unless no valid moves exist, in which case your game ends."
     , " Other Greed commands are 'p' to toggle the highlighting of"
     , " possible moves, '?' to view this message, and 'q' to quit." ] )

handleEvent :: GreedState -> BrickEvent GreedStateName Progression -> EventM GreedStateName (Next GreedState)
handleEvent gs (AppEvent Progression) = checkFinaleE gs
handleEvent gs (VtyEvent (V.EvKey (V.KChar 'p') []))      = continue $ toggleShowPlans gs
handleEvent gs (VtyEvent (V.EvKey (V.KChar 'q') []))      = continue $ quit gs
handleEvent gs (VtyEvent (V.EvKey (V.KChar 'y') []))      = continue $ confirm gs
handleEvent gs (VtyEvent (V.EvKey (V.KChar '?') []))      = continue $ seekHelp gs
handleEvent gs (VtyEvent (V.EvKey k []))
  | k == V.KUp || k == V.KChar 'k' || k == V.KChar '8'    = continue $ increment N gs
  | k == V.KDown || k == V.KChar 'j' || k == V.KChar '2'  = continue $ increment S gs
  | k == V.KRight || k == V.KChar 'l' || k == V.KChar '6' = continue $ increment E gs
  | k == V.KLeft || k == V.KChar 'h' || k == V.KChar '4'  = continue $ increment W gs
  | k == V.KChar 'y' || k == V.KChar '7'                  = continue $ increment NW gs
  | k == V.KChar 'u' || k == V.KChar '9'                  = continue $ increment NE gs
  | k == V.KChar 'b' || k == V.KChar '1'                  = continue $ increment SW gs
  | k == V.KChar 'n' || k == V.KChar '3'                  = continue $ increment SE gs
handleEvent gs _ = continue $ anyKeyPress gs

checkFinaleE :: GreedState -> EventM GreedStateName (Next GreedState)
checkFinaleE gs = case view greedState gs of
  Finale -> halt gs
  _ -> continue gs

drawUI :: GreedState -> [Widget GreedStateName]
drawUI gs = [ hLimit 79 $ vLimit 23 $ gridWidget gs <=> dialogue gs ]

initialState :: IO GreedState
initialState = do
  let rows = 22
  let cols = 79 
  sample <- (replicateM (cols*rows) . randomRIO) (1,9) :: IO [Int]

  let tiles = map (Tile Merely) sample
  let initialNodes = splitAtCols cols tiles
  let flattened = concat initialNodes
  let keys      = listArray ((0,0),(cols-1,rows-1)) flattened
  
  pr <- randomRIO (0, rows-1) :: IO Int
  pc <- randomRIO (0, cols-1) :: IO Int

  let pos = (pc, pr)

  let unmarkedState = GreedState keys pos 0 False Greediness
  let finalState = posOverState (set tileState Marked) pos unmarkedState
  
  return finalState

app :: App GreedState Progression GreedStateName
app = App { appDraw = drawUI
          , appHandleEvent = handleEvent
          , appAttrMap = const defaultMap
          , appStartEvent = return
          , appChooseCursor = neverShowCursor }
