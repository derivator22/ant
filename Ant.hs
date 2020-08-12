module Ant where

import qualified Data.Map.Strict as M

import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Simulate 

type Grid = M.Map Position CellState

data CColor = White | Green

data CellState = CS { _color :: CColor, _visit :: Bool }

data Orientation = North | South | West | East

data Command = L | R | N | U

type Position = (Int, Int)

data Ant = Ant { _position    :: Position,
                 _orientation :: Orientation,
                 _state       :: Int,
                 _steps       :: Integer }

data Universe = US { _ant  :: Ant, _grid :: Grid }

runEvolution = simulate (InWindow "test" (500,500) (0,0))
                        white
                        10
                        initState 
                        renderer
                        updater

evalLoop :: Universe -> Universe    
evalLoop (US ant gr) = US (updateAnt ant col) (updateGrid gr pos)
  where pos = _position ant
        col = _color $  gr M.! pos  

updater :: ViewPort -> Float -> Universe -> Universe
updater _ _ = evalLoop 

updateAnt :: Ant -> CColor -> Ant
updateAnt (Ant (x,y) o s st) Green  =
  case o of
    North -> (Ant (x-1,y) West s (st+1))
    South -> (Ant (x+1,y) East s (st+1))
    West  -> (Ant (x,y-1) South s (st+1))
    East  -> (Ant (x,y+1) North s (st+1))
updateAnt (Ant (x,y) o s st) White =
  case o of
    North -> (Ant (x+1,y) East s (st+1))
    South -> (Ant (x-1,y) West s (st+1))
    West  -> (Ant (x,y+1) North s (st+1))
    East  -> (Ant (x,y-1) South s (st+1))       

updateGrid :: Grid -> Position -> Grid
updateGrid gr pos = M.update (\s -> Just $ upSt s ) pos gr
  where upSt (CS White _) = CS Green True
        upSt (CS Green _) = CS White True
    
initState :: Universe
initState =
  let grid  = M.fromList $ [((x,y), CS White False)
                           | x <- [(-14)..14],
                             y <- [(-14)..14]]
      ant   = Ant (0,0) West 0 0 
  in US ant grid

windowSize = both (* (round cellSize)) fieldSize

fieldSize@(fieldWidth, fieldHeight) = (20, 20) :: (Int, Int)

cellSize = 22

both f (a,b) = (f a, f b)

cellToScreen = both ((* cellSize) . fromIntegral)

initGrid = 
  [color black $ uncurry rectangleWire $ cellToScreen (x, fieldHeight)
    | x <- [0..fieldWidth]] ++
  [color black $ uncurry rectangleWire $ cellToScreen (fieldWidth, x)
    | x <- [0..fieldHeight]]
  
renderer :: Universe -> Picture
renderer (US ant gr) = pictures $
  map cellToPicture (M.toList $ M.filter ((==True) . _visit) gr) ++
  initGrid
    
cellToPicture :: (Position, CellState) -> Picture
cellToPicture ((x,y), CS c v) = color (toGlossColor c) $
  polygon [(x', y'), (x', y'+step), (x'+step, y'+step), (x'+step, y')]
    where (x', y') = both ((*step) . fromIntegral) (x,y)
          step =  0.5 * cellSize
          toGlossColor White = white
          toGlossColor Green = green 

          


 






               
  
  
  
  
  




