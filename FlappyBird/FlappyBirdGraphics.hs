module FlappyBird.FlappyBirdGraphics (
    drawTube
) where

import Flow ( (.>) )

import Helpers.FunctionHelper ( applyWhile )

import ASCIIscreen.ASCIIscreen
    ( hight,
      newASCIIscreen,
      Coords,
      Hight,
      Width,
      ASCIIscreen,
      Screenable(..),
      X_coord,
      Y_coord )
import ASCIIscreen.ASCIIscreenEdit
    ( putRectangle, putRectangleWithEdges )
import FlappyBird.FlappyBirdLogic
    ( GameStatus(flappySize, flappyPos_y, flappyPos_x,
                 halfHoleTubeWide, halfTubeWidth, offsetTubes, firstTube_x,
                 holesTubes_y, screenSize),
      screenWidth )


{- drawTube:
    Draws a tube with the center of the hole in (x, y) a with of 2*hWith, and a hole of size 2*hHoleWith
-}
drawTube :: Width -> Hight -> Coords -> ASCIIscreen -> ASCIIscreen 
drawTube hWith hHoleWith (x, y) screen = 
    putRectangleWithEdges tubeTexture (leftPartTube, bottomPartHole) (rightPartTube, hight screen)
    .> putRectangleWithEdges tubeTexture (leftPartTube, -1) (rightPartTube, topPartHole)
    $ screen
    where
        leftPartTube = x - hWith
        rightPartTube = x + hWith
        bottomPartHole = y + hHoleWith + 1
        topPartHole = y - hHoleWith - 1
        tubeTexture = '#'


drawTubes :: GameStatus -> ASCIIscreen -> ASCIIscreen
drawTubes game =
    applyWhile ((<= screenWidth game) . fst) tubesCenters (\(x, y) ->
        drawTube (halfTubeWidth game) (halfHoleTubeWide game) (x, y)
    )
    where
        _holesTubes_y = holesTubes_y game :: [Y_coord]
        _firstTube_x = round $ firstTube_x game :: X_coord
        _offsetTubes = offsetTubes game :: Width
        tubesCenters :: [Coords]
        tubesCenters =
            zip
                (iterate (+ _offsetTubes) _firstTube_x)
                _holesTubes_y


drawFlappy :: GameStatus -> ASCIIscreen -> ASCIIscreen 
drawFlappy game = putRectangle '@' (x_pos, y_pos - fSize) (x_pos + fSize, y_pos) 
    where
        x_pos = round $ flappyPos_x game
        y_pos = round $ flappyPos_y game
        fSize = flappySize game


drawGame :: GameStatus -> ASCIIscreen -> ASCIIscreen 
drawGame game =
    drawTubes game
    .> drawFlappy game



instance Screenable GameStatus where
    toASCIIscreen game = drawGame game $ newASCIIscreen $ screenSize game







