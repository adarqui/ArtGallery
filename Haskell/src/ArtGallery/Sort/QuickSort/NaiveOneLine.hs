{- source: http://www.haskell.org/haskellwiki/Introduction#Quicksort_in_Haskell -}

module ArtGallery.Sort.QuickSort.NaiveOneLine (
 qsort
) where

qsort (p:xs) = qsort [x | x<-xs, x<p] ++ [p] ++ qsort [x | x<-xs, x>=p]
