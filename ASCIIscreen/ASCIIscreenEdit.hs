{- Iván Renison -}

module ASCIIscreen.ASCIIscreenEdit (
    editHorizontalLine, putHorizontalLine,
    editVerticalLine, putVerticalLine,
    editRectangle, putRectangle,
    editRectangleWithEdges, putRectangleWithEdges,
    putStringIn
) where

import Control.Monad ( forM )
import Control.Monad.Trans.State ( execState, modify )

import ASCIIscreen.ASCIIscreen
    ( editAt, putAt, ASCIIscreen, Coords, X_coord, Y_coord ) 
    
editHorizontalLine :: (Char -> Char) -> Y_coord -> X_coord -> X_coord -> ASCIIscreen -> ASCIIscreen
editHorizontalLine f y x0 x1 = execState $ do
    forM [x0..x1] $ \x -> 
        modify $ editAt f (x, y)
     

putHorizontalLine :: Char -> Y_coord -> X_coord -> X_coord -> ASCIIscreen -> ASCIIscreen
putHorizontalLine c = editHorizontalLine $ const c


editVerticalLine :: (Char -> Char) -> X_coord -> Y_coord -> Y_coord -> ASCIIscreen -> ASCIIscreen
editVerticalLine f x y0 y1 = execState $ do
    forM [y0..y1] $ \y ->
        modify $ editAt f (x, y)

putVerticalLine ::  Char -> X_coord -> Y_coord -> Y_coord -> ASCIIscreen -> ASCIIscreen
putVerticalLine c = editVerticalLine $ const c


editRectangle :: (Char -> Char) -> Coords -> Coords -> ASCIIscreen -> ASCIIscreen
editRectangle f (x0, y0) (x1, y1) = execState $ do
    forM [y0..y1] $ \y ->
        modify $ editHorizontalLine f y x0 x1

putRectangle :: Char -> Coords -> Coords -> ASCIIscreen -> ASCIIscreen
putRectangle c = editRectangle $ const c


editRectangleWithEdges :: (Char -> Char) -> Coords -> Coords -> ASCIIscreen -> ASCIIscreen
editRectangleWithEdges f (x0, y0) (x1, y1) = execState $ do
    modify $ putAt corner (x0, y0)
    modify $ putHorizontalLine horizontalBorder y0 (x0 + 1) (x1 - 1)
    modify $ putAt corner (x1, y0)
    modify $ putVerticalLine verticalBorder x0 (y0 + 1) (y1 - 1)
    modify $ putAt corner (x0, y1)
    modify $ editRectangle f (x0 + 1, y0 + 1) (x1 - 1, y1 - 1)
    modify $ putHorizontalLine horizontalBorder y1 (x0 + 1) (x1 - 1)
    modify $ putVerticalLine verticalBorder x1 (y0 + 1) (y1 - 1)
    modify $ putAt corner (x1, y1)
    where
        verticalBorder = '|'
        horizontalBorder = '-'
        corner = '+'

putRectangleWithEdges :: Char -> Coords -> Coords -> ASCIIscreen -> ASCIIscreen
putRectangleWithEdges c = editRectangleWithEdges $ const c



putStringIn :: String -> Coords -> ASCIIscreen -> ASCIIscreen
putStringIn ss (x, y) = execState $ do
    forM lines_ss $ \(i, cs) ->
        modify $ putLineIn cs (x, y + i)
    where
        lines_ss = zip [0,1..] $ lines ss
        putLineIn :: String -> Coords -> ASCIIscreen -> ASCIIscreen -- puts xs assuming that it has no '\n'
        putLineIn cs (x, y) = execState $ do
            forM chars_cs $ \(j, c) ->
                modify $ putAt c (x + j, y)
            where
                chars_cs = zip [0,1..] cs

