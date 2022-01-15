module FlappyBird.FlappyBirdGraphics () where

import Control.Monad.Extra ( forM_ )
import Control.Monad.Trans.State ( execState, State )
import Data.Fixed ( Pico )
import Numeric.Natural ( Natural )


import ASCIIscreen.ASCIIscreen
    ( Screenable(..),
      ASCIIscreen,
      Coords,
      Y_coord,
      X_coord,
      Hight,
      Width,
      getHight,
      newASCIIscreen )
import ASCIIscreen.ASCIIscreenEdit
    ( s_putRectangle, s_putRectangleWithEdges, s_putStringIn )
import FlappyBird.FlappyBirdLogic
    ( GameStatus(screenSize, holesTubes_y, firstTube_x, halfTubeWidth,
                 halfHoleTubeWide, flappyPos_x, flappyPos_y, flappySize,
                 distanceFromStart, starting_firstTube_x, offsetTubes),
      screenWidth )


{- drawTube:
    Draws a tube with the center of the hole in (x, y) a with of 2*hWith, and a hole of size 2*hHoleWith
-}
drawTube :: Width -> Hight -> Coords -> State ASCIIscreen ()
drawTube hWith hHoleWith (x, y) = do
    hight_screen <- getHight
    s_putRectangleWithEdges tubeTexture (leftPartTube, bottomPartHole) (rightPartTube, hight_screen)
    s_putRectangleWithEdges tubeTexture (leftPartTube, -1) (rightPartTube, topPartHole)
    where
        leftPartTube = x - hWith
        rightPartTube = x + hWith
        bottomPartHole = y + hHoleWith + 1
        topPartHole = y - hHoleWith - 1
        tubeTexture = '#'


drawTubes :: GameStatus -> State ASCIIscreen ()
drawTubes game =
    forM_ tubesCenters $ \(x, y) -> 
        drawTube (halfTubeWidth game) (halfHoleTubeWide game) (x, y)
    where
        _holesTubes_y = holesTubes_y game :: [Y_coord]
        _firstTube_x = round $ firstTube_x game :: X_coord
        _offsetTubes = offsetTubes game :: Width
        tubesCenters :: [Coords]
        tubesCenters =
            zip
                (takeWhile (<= screenWidth game) $ iterate (+ _offsetTubes) _firstTube_x) -- Just draw the tubes that are in the creen
                _holesTubes_y


drawFlappy :: GameStatus -> State ASCIIscreen ()
drawFlappy game = s_putRectangle '@' (x_pos, y_pos - fSize) (x_pos + fSize, y_pos) 
    where
        x_pos = round $ flappyPos_x game
        y_pos = round $ flappyPos_y game
        fSize = flappySize game


printPoints :: GameStatus -> State ASCIIscreen ()
printPoints game = do
    s_putStringIn (show points) (1, 1)
    where
        points = toNatural $ (distanceFromStart game - starting_firstTube_x game) / fromIntegral (offsetTubes game) + 1
        toNatural :: Pico -> Natural
        toNatural n
            | n < 0 = 0
            | otherwise = ceiling n


drawGame :: GameStatus -> State ASCIIscreen ()
drawGame game = do
    drawTubes game
    drawFlappy game
    printPoints game



instance Screenable GameStatus where
    toASCIIscreen game = execState (drawGame game) $ newASCIIscreen $ screenSize game







