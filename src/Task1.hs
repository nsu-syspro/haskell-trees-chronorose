{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (foldl, foldr)

-- * Type definitions

-- | Binary tree
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Show)

-- | Forest (i.e. list of 'Tree's)
type Forest a = [Tree a]

-- | Tree traversal order
data Order = PreOrder | InOrder | PostOrder
  deriving (Show)

-- * Function definitions

-- | Returns values of given 'Tree' in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> torder PreOrder  (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "A.B.."
-- >>> torder InOrder   (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- ".A.B."
-- >>> torder PostOrder (Just '.') (Branch 'A' Leaf (Branch 'B' Leaf Leaf))
-- "...BA"
torder ::
  -- | Order of resulting traversal
  Order ->
  -- | Optional leaf value
  Maybe a ->
  -- | Tree to traverse
  Tree a ->
  -- | List of values in specified order
  [a]
torder ord opt tree = case tree of
  Leaf -> maybeToList opt
  Branch a t1 t2 -> f ord a t1 t2
  where
    toOrder = torder ord opt
    f PreOrder a t1 t2 = [a] ++ toOrder t1 ++ toOrder t2
    f InOrder a t1 t2 = toOrder t1 ++ [a] ++ toOrder t2
    f PostOrder a t1 t2 = toOrder t1 ++ toOrder t2 ++ [a]

-- | Returns values of given 'Forest' separated by optional separator
-- where each 'Tree' is traversed in specified 'Order' with optional leaf value
--
-- Usage example:
--
-- >>> forder PreOrder  (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|C..|A.B.."
-- >>> forder InOrder   (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|.C.|.A.B."
-- >>> forder PostOrder (Just '|') (Just '.') [Leaf, Branch 'C' Leaf Leaf, Branch 'A' Leaf (Branch 'B' Leaf Leaf)]
-- ".|..C|...BA"
forder ::
  -- | Order of tree traversal
  Order ->
  -- | Optional separator between resulting tree orders
  Maybe a ->
  -- | Optional leaf value
  Maybe a ->
  -- | List of trees to traverse
  Forest a ->
  -- | List of values in specified tree order
  [a]
forder ord optF optT = foldr lambda []
  where
    toOrder = torder ord optT
    lambda tree [] = toOrder tree
    lambda tree acc = toOrder tree ++ maybeToList optF ++ acc

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f start ys =
  let go _ acc [] = acc
      go fn acc (x : xs) = fn x (go fn acc xs)
   in go f start ys

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f start ys =
  let go _ acc [] = acc
      go fn acc (x : xs) = go fn (fn acc x) xs
   in go f start ys

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []
