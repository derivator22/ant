module GlossExample where

import Graphics.Gloss

test01 = display (InWindow "test" (500,500) (0,0))
                 (greyN 0.25)
                 (rectangleSolid 250.0 250.0)

{-
renderer = pictures [uncurry translate (cellToScreen (x, y))
                                       $ color black
                                       $ rectangleWire cellSize cellSize
                     | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]
-}

cellToScreen = both ((* cellSize) . fromIntegral)

both f (a, b) = (f a, f b)

cellSize = 30

fieldSize@(fieldWidth, fieldHeight) = (15, 15) :: (Int, Int)

grid (x,y) = pictures $
  [color black $ uncurry rectangleWire $ cellToScreen (w, 15)
    | w <- [0..15]] ++
  [color black $ uncurry rectangleWire $ cellToScreen (15, h)
    | h <- [0..15]] ++
  [color green $ polygon [(x,y), (x,y+15), (x+15,y+15), (x+15,y)]]
                  
testGrid = display (InWindow "test" (500,500) (0,0))
                   white
                   (grid (15*14, 15*14))

test03 = display (InWindow "test" (500,500) (0,0))
                 white
                 (color green $ rectangleSolid 10 20)


type Model = (Float,Float)

renderer :: Model -> Picture
renderer = grid 

updater _ _ (x,y) = (x,y+30)                         

test04 = simulate (InWindow "test" (500,500) (0,0))
                  white
                  5
                  (0,0)
                  renderer
                  updater


