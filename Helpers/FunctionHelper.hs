{- Iván Renison -}

module Helpers.FunctionHelper (
    (.>),
    applyAll,
    applyFor,
    applyWhile,
    applyWhen
) where

import Flow ( (.>) )

import Helpers.FoldableHelper ( foldlWhile )


applyAll :: Foldable t => t (a -> a) -> a -> a
applyAll = foldl (.>) id


applyFor :: Foldable t => t a -> (a -> b -> b) -> b -> b
applyFor xs f = foldl (\g a -> g .> f a) id xs



applyWhile :: Foldable t => (a -> Bool) -> t a -> (a -> b -> b) -> b -> b
applyWhile p xs f = foldlWhile p (\g a -> g .> f a) id xs



applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen False _ = id
applyWhen True f = f

