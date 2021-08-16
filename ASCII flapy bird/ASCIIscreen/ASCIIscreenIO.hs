{- Iván Renison -}

module ASCIIscreen.ASCIIscreenIO (
    printASCIIscreen, rePrintASCIIscreen,
    printInASCIIscreen, rePrintInASCIIscreen
) where

import Control.DeepSeq ( rnf )
import System.Console.ANSI ( setCursorPosition  )

import ASCIIscreen.ASCIIscreen ( Screenable(..), ASCIIscreen )

{- printASCIIscreen
    first reduces the string to be print to normal form for then been able yo print it instantly
-}
printASCIIscreen :: ASCIIscreen -> IO ()
printASCIIscreen s = rnf show_s `seq` putStr show_s
    where show_s = show s

rePrintASCIIscreen :: ASCIIscreen -> IO ()
rePrintASCIIscreen s = rnf show_s `seq` do
    setCursorPosition 0 0
    putStr show_s
    where show_s = show s


printInASCIIscreen :: Screenable g => g -> IO ()
printInASCIIscreen = printASCIIscreen . toASCIIscreen

rePrintInASCIIscreen :: Screenable q => q -> IO ()
rePrintInASCIIscreen = rePrintASCIIscreen . toASCIIscreen


