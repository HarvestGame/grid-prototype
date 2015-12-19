-- Example of using the Canvas library to render simple tile maps.
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas as Canvas
import Haste.Graphics.AnimationFrame
import Data.IORef

import Debug.Trace

import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

data Terrain = Red | Blue | Yellow | White | Black deriving (Ord, Eq)

data Grid = Grid {
  terrainMap :: Map.Map (Int, Int) Terrain,
  width :: Int,
  height :: Int
}

data Entity = Building {
  lowerLeft :: (Int, Int),
  size :: (Int, Int)
} deriving Show

data State = State {
  position :: (Int, Int),
  pressedKeys :: IntSet.IntSet,
  lastUpdate :: HRTimeStamp,
  rawMousePosition :: Maybe (Int, Int),
  entities :: [Entity],
  entityGrid :: Map.Map (Int, Int) [Entity]
} deriving Show

mousePosition :: State -> Maybe (Int, Int) 
mousePosition state = fmap helper (rawMousePosition state) where 
  helper :: (Int, Int) -> (Int, Int)
  helper (x, y) = (x - xPosition, y - yPosition) where
    (xPosition, yPosition) = (position state)

cellWidth :: Double
cellHeight :: Double

cellWidth = 10.0
cellHeight = 10.0

gridHeight :: Int
gridWidth :: Int

gridHeight = 30
gridWidth = 30

colorMap :: Map.Map Terrain Canvas.Color
colorMap = Map.fromList [(Red, Canvas.RGB 255 0 0), (Black, Canvas.RGB 0 0 0)]

-- From http://stackoverflow.com/questions/9722689/haskell-how-to-map-a-tuple
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

drawGrid :: Grid -> Canvas.Picture ()
drawGrid grid = sequence_ eachPosition where
  eachPosition :: [Picture ()]
  eachPosition = do
    x <- [0..(width grid)]          
    y <- [0..(height grid)]        
    return $ drawPosition (x, y) grid

pixelToGrid :: Int -> Int
pixelToGrid pixel = floor $ ((fromIntegral pixel)/10 ::Double)

drawMouse :: Maybe (Int, Int) -> Canvas.Picture ()
drawMouse Nothing = return ()
drawMouse (Just (x, y)) = Canvas.fill $ Canvas.rect (doubleX*cellWidth, doubleY*cellHeight) ((doubleX+1)*cellWidth, (doubleY+1)*cellHeight) where
  (doubleX, doubleY) = mapTuple (fromIntegral . pixelToGrid) (x, y)

toPixels :: (Int, Int) -> (Double, Double) 
toPixels = mapTuple (\a -> cellWidth * fromIntegral a)

drawBuilding :: Entity -> Canvas.Picture ()
drawBuilding (Building{lowerLeft=(x,y), size=(dx, dy)}) = Canvas.fill $ Canvas.rect (toPixels (x,y)) $ toPixels (x+dx, y+dy) 

createSimpleBuilding :: (Int, Int) -> Entity
createSimpleBuilding (x, y) = Building {
  lowerLeft = (x - 1, y),
  size = (3, 2)
}

isValidPlacement :: Entity -> State -> Bool
isValidPlacement (Building{lowerLeft=(x,y), size=(dx, dy)}) state = and validPositions where
  validPositions :: [Bool]
  validPositions = do 
    xPos <- [x..(dx+x)]
    yPos <- [y..(dy+y)]
    if (xPos <0) || (xPos >= gridWidth) || (yPos <0) || (yPos >= gridHeight) then
      return False
    else
      return True

currentTryToPlaceBuilding :: State -> Maybe Entity 
currentTryToPlaceBuilding state = if (isTryingToBuild state) && (Maybe.isJust (mousePosition state)) then
    Just $ createSimpleBuilding mouseGrid
  else
    Nothing 
  where
    mouseGrid = mapTuple pixelToGrid $ Maybe.fromJust (mousePosition state)

renderTryingToBuild :: Entity -> State -> Canvas.Picture ()
renderTryingToBuild building state = do
  let isValid = isValidPlacement building state
  let buildingColor = if isValid then
        Canvas.RGB 0 255 0
      else
        Canvas.RGB 255 0 0
  Canvas.color buildingColor $ drawBuilding building

renderEverything :: Canvas.Canvas -> Grid -> State -> IO ()
renderEverything canvas grid state =  Canvas.render canvas $ Canvas.translate doublePosition picture where 
  picture = do 
    drawGrid grid
    maybe (drawMouse (mousePosition state)) (\building -> renderTryingToBuild building state) $ currentTryToPlaceBuilding state
    return ()
  doublePosition = mapTuple fromIntegral (position state)

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
b :: Int

b = 66

up = 38
down = 40
left = 37
right = 39


isTryingToBuild :: State -> Bool
isTryingToBuild (State{pressedKeys = thePressedKeys}) = IntSet.member b thePressedKeys

updatePosition :: (Int, Int) -> IntSet.IntSet -> (Int, Int) 
updatePosition (x,y) pressedKeys = 
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

updateState :: State -> State
updateState state = state {
  lastUpdate = (lastUpdate state) + 15.0,
  position = updatePosition (position state) (pressedKeys state)
}

keepUpdatingStateUntilCurrent :: HRTimeStamp -> State -> State
keepUpdatingStateUntilCurrent current_time last_state = 
  if ((lastUpdate last_state) < current_time) then
    keepUpdatingStateUntilCurrent  current_time $ updateState last_state 
  else
    last_state  

renderAndUpdate :: Canvas.Canvas -> Grid -> IORef State -> HRTimeStamp -> IO ()
renderAndUpdate canvas grid state time = do 
  modifyIORef state $ keepUpdatingStateUntilCurrent time
  currentState <- readIORef state
  renderEverything canvas grid currentState
  _ <- requestAnimationFrame $ renderAndUpdate canvas grid state
  return ()

pressKey :: Int -> State -> State
pressKey code state = state {pressedKeys = IntSet.insert code (pressedKeys state)}

releaseKey :: Int -> State -> State
releaseKey code state = state {pressedKeys = IntSet.delete code (pressedKeys state)}

handleClick :: State -> State
handleClick state = trace (show $ state) state

main :: IO ()
main = do
  -- Setup: get the HTML element for a canvas, then proceed to create a canvas
  -- object from it.

  Just ce <- elemById "canvas"
  Just c <- fromElem ce
  
  -- Start the application with the top left element selected and a 10*10 map.
  state <- newIORef $ State {
      position = (0, 0),
      pressedKeys  = IntSet.empty,
      lastUpdate = 0.0,
      rawMousePosition = Nothing,
      entities = [],
      entityGrid = Map.empty
    }

  let grid = Grid {terrainMap = Map.fromList [((0,0), Red)], width = 30, height = 30}

  _ <- document `onEvent` KeyDown $ (\evt -> modifyIORef state $ pressKey (keyCode evt))
  _ <- document `onEvent` KeyUp $ (\evt -> modifyIORef state $ releaseKey (keyCode evt))
  _ <- ce `onEvent` MouseMove $ (\evt -> modifyIORef state $ (\st -> st {rawMousePosition = Just $ mouseCoords evt}))
  _ <- ce `onEvent` MouseOut $ (\_ -> modifyIORef state $ (\st -> st {rawMousePosition = Nothing}))
  _ <- ce `onEvent` MouseDown $ (\_ -> modifyIORef state handleClick)
  _ <- requestAnimationFrame $ renderAndUpdate c grid state
  
  return ()