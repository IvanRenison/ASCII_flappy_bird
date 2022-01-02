module FlappyBird.FlappyBirdGraphics () where

import Control.Monad.Extra ( forM_ )
import Control.Monad.Trans.State ( execState, State )


import ASCIIscreen.ASCIIscreen
    ( Screenable(..),
      ASCIIscreen,
      Coords,
      Y_coord,
      X_coord,
      Hight,
      Width,
      newASCIIscreen,
      getHight )
import ASCIIscreen.ASCIIscreenEdit
    ( s_putRectangle, s_putRectangleWithEdges )
import FlappyBird.FlappyBirdLogic
    ( GameStatus(flappySize, flappyPos_y, flappyPos_x,
                 halfHoleTubeWide, halfTubeWidth, offsetTubes, firstTube_x,
                 holesTubes_y, screenSize),
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
--    applyWhile ((<= screenWidth game) . fst) tubesCenters (\(x, y) ->
--        execState $ drawTube (halfTubeWidth game) (halfHoleTubeWide game) (x, y)
--    )
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


drawGame :: GameStatus -> State ASCIIscreen ()
drawGame game = do
    drawTubes game
    drawFlappy game



instance Screenable GameStatus where
    toASCIIscreen game = execState (drawGame game) $ newASCIIscreen $ screenSize game







