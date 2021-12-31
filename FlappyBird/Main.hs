{-# LANGUAGE NondecreasingIndentation #-}

import Control.Concurrent ( threadDelay )
import System.Console.ANSI
    ( getTerminalSize, hideCursor, showCursor )
import System.IO ( stdin, hReady, hSetBuffering, BufferMode(NoBuffering) )
import System.Random ( newStdGen )
import Data.Maybe ( fromJust, isNothing )
import Data.List.Extra ( notNull )
import Data.Time.Clock
    ( NominalDiffTime, diffUTCTime, getCurrentTime, UTCTime ) 
import Data.Tuple ( swap )

import Helpers.TimeHelper ( nominalDiffTimeToMicroSeconds )

import ASCIIscreen.ASCIIscreenIO
    ( printInASCIIscreen, rePrintInASCIIscreen ) 
import FlappyBird.FlappyBirdLogic
    ( GameStatus, newGame, updateGame, hasEnded )
import FlappyBird.FlappyBirdGraphics ()




{- main:
    obtains terminal size
    starts a new game
    calls gameLoop
-}
main :: IO ()
main = do
    game <- startGame
    gameLoop game



startGame :: IO GameStatus
startGame = do
    maybeSize <- getTerminalSize
    stdGen <- newStdGen
    if isNothing maybeSize then
        error "ERROR: Unable to obtain terminal size"
    else do
    let (w, h) = swap $ fromJust maybeSize
        game = newGame stdGen (w, h-1)
    return game


{- gameLoop:
    loop that runs during game-play
-}
gameLoop :: GameStatus -> IO ()
gameLoop game = do
    hideCursor
    hSetBuffering stdin NoBuffering
    time <- getCurrentTime
    printInASCIIscreen game
    loop time game
    where
        loop :: UTCTime -> GameStatus -> IO ()
        loop lastTime game = do
            time <- getCurrentTime
            let deltaTime = time `diffUTCTime` lastTime :: NominalDiffTime
                minTimePerFrame = 1/60 :: NominalDiffTime -- realToFrac to a NominalDiffTime treats numbers as seconds
                delay = max (minTimePerFrame - deltaTime) 0
            threadDelay $ nominalDiffTimeToMicroSeconds delay
            buffer <- getBuffer
            if '\EOT' `elem` buffer then exitGame game
            else do
            let jump = notNull buffer
                game' = updateGame deltaTime jump game
            rePrintInASCIIscreen game'
            if hasEnded game' then exitGame game'
            else do
            loop time game'




exitGame :: GameStatus -> IO ()
exitGame _ = showCursor



getBuffer :: IO String
getBuffer = do
    b <- hReady stdin
    if not b then return ""
    else do
    c <- getChar
    cs <- getBuffer
    return (c:cs)

