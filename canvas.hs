-- Example of using the Canvas library to render simple tile maps.
import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas as Canvas
import Haste.Graphics.AnimationFrame
import Data.IORef
import Data.Array
import Data.Array.MArray
import Data.Array.IO

import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

data Terrain = Red | Blue | Yellow | White | Black deriving (Ord, Eq)

data Grid = Grid {
  terrainMap :: Map.Map (Int, Int) Terrain,
  width :: Int,
  height :: Int
}

data State = State {
  position :: (Double, Double),
  keyState :: IntSet.IntSet,
  lastUpdate :: HRTimeStamp
}

cellWidth :: Double
cellHeight :: Double

cellWidth = 10.0
cellHeight = 10.0

colorMap :: Map.Map Terrain Canvas.Color
colorMap = Map.fromList [(Red, Canvas.RGB 255 0 0), (Black, Canvas.RGB 0 0 0)]

drawGrid :: Grid -> Canvas.Picture ()
drawGrid grid = sequence_ eachPosition where
  eachPosition :: [Picture ()]
  eachPosition = do
    x <- [0..(width grid)]          
    y <- [0..(height grid)]        
    return $ drawPosition (x, y) grid

renderEverything :: Canvas.Canvas -> Grid -> State -> IO ()
renderEverything canvas grid state = Canvas.render canvas $ translate (position state) $ drawGrid grid

drawPosition :: (Int, Int) -> Grid -> Canvas.Picture ()
drawPosition (x, y) grid = Canvas.color terrainColor $ Canvas.stroke square where
  doubleX = fromIntegral x
  doubleY = fromIntegral y
  terrain :: Terrain
  terrain = Map.findWithDefault Black (x, y) (terrainMap grid) 
  terrainColor = (Map.!) colorMap terrain
  square = Canvas.rect (doubleX*cellWidth, doubleY*cellHeight) ((doubleX+1)*cellWidth, (doubleY+1)*cellHeight)

up :: Int
down :: Int
right :: Int
left :: Int

up = 38
down = 40
left = 37
right = 39

update_position :: (Double, Double) -> IntSet.IntSet -> (Double, Double) 
update_position (x,y) pressedKeys = 
  if IntSet.member up pressedKeys then
    (x, y+1)
  else if IntSet.member down pressedKeys then
    (x, y-1)
  else if IntSet.member right pressedKeys then
    (x-1, y)
  else if IntSet.member left pressedKeys then
    (x+1, y)
  else
    (x, y)

update_state :: State -> State
update_state state = state {
  lastUpdate = (lastUpdate state) + 15.0,
  position = update_position (position state) (keyState state)
}

keep_updating_state_until_current :: HRTimeStamp -> State -> State
keep_updating_state_until_current current_time last_state = 
  if ((lastUpdate last_state) < current_time) then
    keep_updating_state_until_current  current_time $ update_state last_state 
  else
    last_state  

renderAndUpdate :: Canvas.Canvas -> Grid -> IORef State -> HRTimeStamp -> IO ()
renderAndUpdate canvas grid state time = do 
  modifyIORef state $ keep_updating_state_until_current time
  current_state <- readIORef state
  renderEverything canvas grid current_state
  _ <- requestAnimationFrame $ renderAndUpdate canvas grid state
  return ()

press_key :: Int -> State -> State
press_key code state = state {keyState = IntSet.insert code (keyState state)}


release_key :: Int -> State -> State
release_key code state = state {keyState = IntSet.delete code (keyState state)}



main :: IO ()
main = do
  -- Setup: get the HTML element for a canvas, then proceed to create a canvas
  -- object from it.

  Just ce <- elemById "canvas"
  Just c <- fromElem ce
  
  -- Start the application with the top left element selected and a 10*10 map.
  state <- newIORef $ State {
      position = (0, 0),
      keyState  = IntSet.empty,
      lastUpdate = 0.0
    }

  let grid = Grid {terrainMap = Map.fromList [((0,0), Red)], width = 10, height = 10}
  putStrLn "Ok"

  _ <- document `onEvent` KeyDown $ (\evt -> modifyIORef state $ press_key (keyCode evt))
  _ <- document `onEvent` KeyUp $ (\evt -> modifyIORef state $ release_key (keyCode evt))
  _ <- requestAnimationFrame $ renderAndUpdate c grid state
  
  return ()