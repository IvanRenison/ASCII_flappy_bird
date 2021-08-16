module ASCIIscreen.ListZipper (
    ListZipper(),
    newListZipper,
    editPos,
    toList
) where

data ListZipper a = ListZipper {
    left :: [a],
    actualPos :: Int,
    right :: [a]
} deriving Show



newListZipper :: [a] -> ListZipper a
newListZipper = ListZipper [] 0

editPos :: Int -> (a -> a) -> ListZipper a -> ListZipper a
editPos p f (ListZipper ls ap rs)
    | p == ap = case rs of
                    [] -> error "index to large"
                    (r:rs') -> ListZipper ls ap (f r : rs')
    | p < ap  = case ls of
                    [] -> error "index to small"
                    (l:ls') -> editPos p f $ ListZipper ls' (ap - 1) (l:rs)
    | otherwise = case rs of
                    [] -> error "index to large"
                    (r:rs') -> editPos p f $ ListZipper (r:ls) (ap + 1) rs'

toList :: ListZipper a -> [a]
toList (ListZipper ls _ rs) = reverse ls ++ rs 


