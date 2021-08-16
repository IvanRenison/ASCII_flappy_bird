module Helpers.FoldableHelper (
    toListWhile,
    foldlWhile
) where



import Data.Foldable ( Foldable(toList) )


toListWhile :: Foldable t => (a -> Bool) -> t a -> [a]
toListWhile p = takeWhile p . toList


foldlWhile :: Foldable t => (a -> Bool) -> (b -> a -> b) -> b -> t a -> b
foldlWhile p f z = foldl f z . toListWhile p


