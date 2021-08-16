module ASCIIscreen.ASCIIscreen (
    Width, Hight, Size,
    X_coord, Y_coord, Coords,
    ASCIIscreen, size, width, hight,
    newASCIIscreen,
    putAt, editAt,
    Screenable(..),
    showAsASCIIscreen
) where

import Data.Sequence.Internal ()


import ASCIIscreen.ListZipper ( editPos, newListZipper, toList, ListZipper ) 

type Width = Int
type Hight = Int
type Size = (Width, Hight)

type X_coord = Int
type Y_coord = Int
type Coords = (X_coord, Y_coord)


data ASCIIscreen = ASCIIscreen Size (ListZipper (ListZipper Char))

size :: ASCIIscreen -> Size
size (ASCIIscreen s _) = s

width :: ASCIIscreen -> Width
width = fst . size

hight :: ASCIIscreen -> Hight
hight = snd . size

instance Show ASCIIscreen where
    show (ASCIIscreen _ lz) = unlines . map toList . toList $ lz


{- newASCIIscreen:
    Creates a screen of size x × y all fill in with ' '
-}
newASCIIscreen :: Size -> ASCIIscreen
newASCIIscreen (w, h) = ASCIIscreen (w, h) $ newListZipper $ replicate h $ newListZipper $ replicate w ' '

{- putAr:
    It replaces the char at position (x, y) be c
    If the position (x, y) is outside of the screen it doesn't do anything
-}
putAt :: Char -> Coords -> ASCIIscreen -> ASCIIscreen
putAt c = editAt $ const c

{- editAt:
    Edits the screen in position (x, y) applying f
    If the position (x, y) is outside of the screen it doesn't do anything
-}
editAt :: (Char -> Char) -> Coords -> ASCIIscreen -> ASCIIscreen
editAt f (x, y) s@(ASCIIscreen (w, h) lz)
    | x < 0 || x >= w || y < 0 || y >= h = s -- Case in witch the point its outside de screen
    | otherwise = ASCIIscreen (w, h) $ editPos y (editPos x f) lz



class Screenable g where
    toASCIIscreen :: g -> ASCIIscreen


showAsASCIIscreen :: Screenable g => g -> String 
showAsASCIIscreen = show . toASCIIscreen




