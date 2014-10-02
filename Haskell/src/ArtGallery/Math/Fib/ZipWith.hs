{- source: http://www.haskell.org/haskellwiki/The_Fibonacci_sequence -}

module ArtGallery.Math.Fib.ZipWith (
 fib,
 fibs
) where

fib n = fibs!!n

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
