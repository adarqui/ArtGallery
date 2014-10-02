{- source: http://www.haskell.org/haskellwiki/Introduction#Quicksort_in_Haskell -}

module ArtGallery.Sort.QuickSort.Naive (
 quicksort
) where

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs
