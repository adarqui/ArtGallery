{- source: http://alexandersgreen.wordpress.com/2010/09/13/prefix-trees-in-haskell/ -}

module ArtGallery.Sort.PrefixTree.AlexandersGreen (
 PFTree (..),
 Key,
 initialTree,
 lookup,
 insert,
 ex
) where

import Prelude hiding (lookup)
import Data.List hiding (lookup, insert)

type Key a = [a]

data PFTree a b = Node (Maybe b) (a -> Maybe (PFTree a b))

initialTree :: PFTree a b
initialTree = Node Nothing (\_ -> Nothing)

lookup :: Key a -> PFTree a b -> Maybe b
lookup [] (Node b _) = b
lookup (x:xs) (Node b f) = case f x of
                           Nothing -> Nothing
                           Just pt -> lookup xs pt

insert :: Eq a => Key a -> b -> PFTree a b -> PFTree a b
insert [] b (Node _ f) = Node (Just b) f
insert (x:xs) b (Node mb f) = case f x of
                            Nothing -> Node mb (\x' -> if x' == x then Just (insert xs b initialTree) else f x')
                            Just pt -> Node mb (\x' -> if x' == x then Just (insert xs b pt) else f x')

-- Example tree from the blog post

ex =
 foldl' (\tree (x,y) -> insert x y tree) initialTree
  [
   ("ab",'N'),("abcd",'X'),("aah",'J'),("aafa",'G'),("ha",'P'),("h",'F'),
   ("hap",'Y'),("hca",'P'),("hef",'P'),("he",'I'),("hed",'G'),("hei",'T'),("hep",'X')
  ]
