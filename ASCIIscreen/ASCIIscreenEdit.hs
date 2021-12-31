{- HLINT ignore "Use camelCase" -}

module ASCIIscreen.ASCIIscreenEdit (
    s_editHorizontalLine, editHorizontalLine,
    s_putHorizontalLine, putHorizontalLine,
    s_editVerticalLine, editVerticalLine,
    s_putVerticalLine, putVerticalLine,
    s_editRectangle, editRectangle,
    s_putRectangle, putRectangle,
    s_editRectangleWithEdges, editRectangleWithEdges,
    s_putRectangleWithEdges, putRectangleWithEdges,
    s_putStringIn, putStringIn
) where

import Control.Monad ( forM_ )
import Control.Monad.Trans.State ( execState, State )

import ASCIIscreen.ASCIIscreen
    ( X_coord, Coords, ASCIIscreen, Y_coord, s_putAt, s_editAt )

s_editHorizontalLine :: (Char -> Char) -> Y_coord -> X_coord -> X_coord -> State ASCIIscreen ()
s_editHorizontalLine f y x0 x1 = forM_ [x0..x1] $ \x -> s_editAt f (x, y)

editHorizontalLine :: (Char -> Char) -> Y_coord -> X_coord -> X_coord -> ASCIIscreen -> ASCIIscreen
editHorizontalLine f y x0 x1 = execState $ s_editHorizontalLine f y x0 x1

s_putHorizontalLine :: Char -> Y_coord -> X_coord -> X_coord -> State ASCIIscreen ()
s_putHorizontalLine c = s_editHorizontalLine $ const c

putHorizontalLine :: Char -> Y_coord -> X_coord -> X_coord -> ASCIIscreen -> ASCIIscreen
putHorizontalLine c y x0 x1 = execState $ s_putHorizontalLine c y x0 x1


s_editVerticalLine :: (Char -> Char) -> X_coord -> Y_coord -> Y_coord -> State ASCIIscreen ()
s_editVerticalLine f x y0 y1 = forM_ [y0..y1] $ \y -> s_editAt f (x, y)

editVerticalLine :: (Char -> Char) -> X_coord -> Y_coord -> Y_coord -> ASCIIscreen -> ASCIIscreen
editVerticalLine f x y0 y1 = execState $ s_editVerticalLine f x y0 y1

s_putVerticalLine :: Char -> X_coord -> Y_coord -> Y_coord -> State ASCIIscreen ()
s_putVerticalLine c = s_editVerticalLine $ const c

putVerticalLine ::  Char -> X_coord -> Y_coord -> Y_coord -> ASCIIscreen -> ASCIIscreen
putVerticalLine c x y0 y1 = execState $ s_putVerticalLine c x y0 y1


s_editRectangle :: (Char -> Char) -> Coords -> Coords -> State ASCIIscreen ()
s_editRectangle f (x0, y0) (x1, y1) =
    forM_ [y0..y1] $ \y ->
        s_editHorizontalLine f y x0 x1

editRectangle :: (Char -> Char) -> Coords -> Coords -> ASCIIscreen -> ASCIIscreen
editRectangle f (x0, y0) (x1, y1) = execState $ s_editRectangle f (x0, y0) (x1, y1)

s_putRectangle :: Char -> Coords -> Coords -> State ASCIIscreen ()
s_putRectangle c = s_editRectangle $ const c

putRectangle :: Char -> Coords -> Coords -> ASCIIscreen -> ASCIIscreen
putRectangle c (x0, y0) (x1, y1) = execState $ s_putRectangle c (x0, y0) (x1, y1)


s_editRectangleWithEdges :: (Char -> Char) -> Coords -> Coords -> State ASCIIscreen ()
s_editRectangleWithEdges f (x0, y0) (x1, y1) = do
    s_putAt corner (x0, y0)
    s_putHorizontalLine horizontalBorder y0 (x0 + 1) (x1 - 1)
    s_putAt corner (x1, y0)
    s_putVerticalLine verticalBorder x0 (y0 + 1) (y1 - 1)
    s_putAt corner (x0, y1)
    s_editRectangle f (x0 + 1, y0 + 1) (x1 - 1, y1 - 1)
    s_putHorizontalLine horizontalBorder y1 (x0 + 1) (x1 - 1)
    s_putVerticalLine verticalBorder x1 (y0 + 1) (y1 - 1)
    s_putAt corner (x1, y1)
    where
        verticalBorder = '|'
        horizontalBorder = '-'
        corner = '+'

editRectangleWithEdges :: (Char -> Char) -> Coords -> Coords -> ASCIIscreen -> ASCIIscreen
editRectangleWithEdges f (x0, y0) (x1, y1) = execState $ s_editRectangleWithEdges f (x0, y0) (x1, y1)

s_putRectangleWithEdges :: Char -> Coords -> Coords -> State ASCIIscreen ()
s_putRectangleWithEdges c = s_editRectangleWithEdges $ const c

putRectangleWithEdges :: Char -> Coords -> Coords -> ASCIIscreen -> ASCIIscreen
putRectangleWithEdges c (x0, y0) (x1, y1) = execState $ s_putRectangleWithEdges c (x0, y0) (x1, y1)


s_putStringIn :: String -> Coords -> State ASCIIscreen ()
s_putStringIn ss (x, y) =
    forM_ lines_ss $ \(i, cs) ->
        s_putLineIn cs (x, y + i)
    where
        lines_ss = zip [0,1..] $ lines ss
        s_putLineIn :: String -> Coords -> State ASCIIscreen () -- puts xs assuming that it has no '\n'
        s_putLineIn cs (x, y) =
            forM_ chars_cs $ \(j, c) ->
                s_putAt c (x + j, y)
            where
                chars_cs = zip [0,1..] cs

putStringIn :: String -> Coords -> ASCIIscreen -> ASCIIscreen
putStringIn ss (x, y) = execState $ s_putStringIn ss (x, y)
