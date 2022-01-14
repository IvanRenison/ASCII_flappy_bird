module FlappyBird.FlappyBirdLogic (
    Jump, Distance, Speed, Acceleration,
    GameStatus(..),
    newGame, updateGame,
    hasEnded, screenWidth
) where

import Data.Fixed ( Pico )
import Data.Time.Clock ( nominalDiffTimeToSeconds, NominalDiffTime )
import System.Random ( randomRs, RandomGen )

import ASCIIscreen.ASCIIscreen ( Coords, Hight, Size, Width, Y_coord )


type Jump = Bool

type Distance = Pico     -- Pixels

type Speed = Pico        -- Pixels / Secund

type Acceleration = Pico -- Pixels / Secund²


data GameStatus = GameStatus {
    -- Change during gameplay
    flappyPos_x :: Distance,          -- most left part of the flappy
    flappyPos_y :: Distance,          -- bottom part of the flappy

    verticalSpeed :: Speed,     

    firstTube_x :: Distance,      

    isAlive :: Bool,

    -- Don't change during gameplay
    holesTubes_y :: [Y_coord],        -- list of the hights of the center of the holes of the tubes

    screenSize :: Size,

    horizontalSpeed :: Speed,   
    jumpSpeed :: Speed,         
    gravity :: Acceleration,           

    flappySize :: Int,             -- the flappy is treated as a square

    offsetTubes :: Width,            -- distance between tubes
    halfTubeWidth :: Width,        
    halfHoleTubeWide :: Hight      
} deriving Show



newGame :: RandomGen a => a -> Size -> GameStatus
newGame seed (w, h) =
    GameStatus {
        flappyPos_x = fromIntegral _offsetTubes,
        flappyPos_y = _flappyPos_y,
        verticalSpeed = 0,
        firstTube_x = fromIntegral $ w `div` 2,

        isAlive = True,

        holesTubes_y = _holesTubes_y (h `div` 2) $ randomRs (-maxDiffTubes, maxDiffTubes) seed,

        screenSize = (w, h),

        horizontalSpeed = -standardSpeed,
        jumpSpeed = -standardSpeed * 0.55,
        gravity = -standardSpeed * 1.3, -- jumpSpeed and gravity are negative becase the x axis increases down

        flappySize = _halfHoleTubeWide `div` 5 + 1, -- + 1 to assure a non 0 size flappy

        offsetTubes = _offsetTubes,
        halfTubeWidth = _offsetTubes `div` 6,
        halfHoleTubeWide = _halfHoleTubeWide
    }
    where
        _flappyPos_y = fromIntegral $ h `div` 2
        _offsetTubes = w `div` 5

        border = h `div` 10 + 1 + _halfHoleTubeWide -- Minimal distance between the border of the screen and the center of a tube
        maxDiffTubes =  _offsetTubes `div` 2  -- Maximum diferencie in hight between two consecutive holes
        _holesTubes_y :: Y_coord -> [Hight] -> [Y_coord]
        _holesTubes_y y_lastTube (diffTube:diffTubes)
            | border <= y_newTube && y_newTube <= h - border = y_newTube : _holesTubes_y y_newTube diffTubes
            | otherwise                                      = _holesTubes_y y_lastTube diffTubes
            where
                y_newTube = y_lastTube + diffTube
        _holesTubes_y _ _ = undefined -- These should never be de case

        _halfHoleTubeWide = h `div` 6
        standardSpeed = fromIntegral w * 0.25




updateGame :: NominalDiffTime -> Jump -> GameStatus -> GameStatus
updateGame deltaTime jump =
    updatePositions
    . deleteUnnecessaryTubes
    . checkCollisions
    {- Tree steps:
        • Update positions
        • Delete no more visible tubes
        • Check collisions
    -}
    where
        updatePositions :: GameStatus -> GameStatus
        updatePositions game = game {
            flappyPos_y = flappyPos_y game + _verticalSpeed * deltaTime_,
            firstTube_x = firstTube_x game + horizontalSpeed game * deltaTime_,
            verticalSpeed = _verticalSpeed
        }
            where
                deltaTime_ = nominalDiffTimeToSeconds deltaTime
                _verticalSpeed
                    | jump      = jumpSpeed game
                    | otherwise = verticalSpeed game - gravity game * deltaTime_

        deleteUnnecessaryTubes :: GameStatus -> GameStatus
        deleteUnnecessaryTubes game = game {
            firstTube_x = _firstTube_x,
            holesTubes_y = _holesTubes_y
        }
            where
                (_firstTube_x, _holesTubes_y)
                    | firstTube_x_ <= - halfTubeWidth_ = (firstTube_x_ + offsetTubes_, tail holesTubes_y_)
                    | otherwise                        = (firstTube_x_               ,      holesTubes_y_)
                firstTube_x_ = firstTube_x game
                holesTubes_y_ = holesTubes_y game
                halfTubeWidth_ = fromIntegral $ halfTubeWidth game
                offsetTubes_ = fromIntegral $ offsetTubes game

        checkCollisions :: GameStatus -> GameStatus
        checkCollisions game
            | hasCollisions game = game {isAlive = False} 
            | otherwise          = game


{- hasCollisions:
    Checks if the flappy is colliding with a tube or with thw border of the screen

    Functioning:
        It first checks in the x direction, for that it check with the first tube,
        if it is at the right of the first tube, it restarts de check with the secund tube,
        if it it is at the left of the first tube it returns False, and if it is "inside"
        the first tube it checks in the y direction
-}
hasCollisions :: GameStatus -> Bool
hasCollisions game =
    let
        _holesTubes_y = holesTubes_y game
        _firstTube_x = round $ firstTube_x game
        _offsetTubes = offsetTubes game
        tubesCenters :: [Coords]
        tubesCenters =
            zip
                (iterate (+ _offsetTubes) _firstTube_x)
                _holesTubes_y
    in
    topFlappy < 0
    || bottomFlappy > screenHight game
    || x_loop tubesCenters
    where
        x_loop :: [Coords] -> Bool
        x_loop ((x,y):xys)
            | rightFlappy < leftTube = False
            | leftFlappy > rightTube = x_loop xys
            | otherwise = not y_checks
            where
                halfTubeWidth_ = halfTubeWidth game
                leftTube = x - halfTubeWidth_
                rightTube = x + halfTubeWidth_

                halfHoleTubeWide_ = halfHoleTubeWide game
                topHoleTube = y - halfHoleTubeWide_
                bottomHoleTube = y + halfHoleTubeWide_

                y_checks = topHoleTube - 1 < topFlappy && bottomFlappy < bottomHoleTube + 1
        x_loop [] = undefined -- These should never happened

        flappySize_ = flappySize game
        leftFlappy = round $ flappyPos_x game
        rightFlappy = leftFlappy + flappySize_
        bottomFlappy = round $ flappyPos_y game
        topFlappy = bottomFlappy - flappySize_




hasEnded :: GameStatus -> Bool
hasEnded = not . isAlive

screenWidth :: GameStatus -> Width
screenWidth = fst . screenSize

screenHight :: GameStatus -> Hight
screenHight = snd . screenSize
