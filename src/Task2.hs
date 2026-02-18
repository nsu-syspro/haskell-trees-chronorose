{-# OPTIONS_GHC -Wall #-}

-- The above pragma enables all warnings

module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment

import Task1 (Order (InOrder), Tree (..), foldr, torder)
import Prelude hiding (Ordering (..), compare, foldl, foldr)

-- * Type definitions

-- | Ordering enumeration
data Ordering = LT | EQ | GT
  deriving (Show, Eq)

-- | Binary comparison function indicating whether first argument is less, equal or
-- greater than the second one (returning 'LT', 'EQ' or 'GT' respectively)
type Cmp a = a -> a -> Ordering

-- * Function definitions

-- | Binary comparison function induced from `Ord` constraint
--
-- Usage example:
--
-- >>> compare 2 3
-- LT
-- >>> compare 'a' 'a'
-- EQ
-- >>> compare "Haskell" "C++"
-- GT
compare :: (Ord a) => Cmp a
compare x y
  | x < y = LT
  | x > y = GT
  | otherwise = EQ

-- | Conversion of list to binary search tree
-- using given comparison function
--
-- Usage example:
--
-- >>> listToBST compare [2,3,1]
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> listToBST compare ""
-- Leaf
listToBST :: Cmp a -> [a] -> Tree a
listToBST cmp = foldr (tinsert cmp) Leaf

-- | Conversion from binary search tree to list
--
-- Resulting list will be sorted
-- if given tree is valid BST with respect
-- to some 'Cmp' comparison.
--
-- Usage example:
--
-- >>> bstToList (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- [1,2,3]
-- >>> bstToList Leaf
-- []
bstToList :: Tree a -> [a]
bstToList = torder InOrder Nothing

-- | Tests whether given tree is a valid binary search tree
-- with respect to given comparison function
--
-- Usage example:
--
-- >>> isBST compare (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- True
-- >>> isBST compare (Leaf :: Tree Char)
-- True
-- >>> isBST compare (Branch 5 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- False
isBST :: Cmp a -> Tree a -> Bool
isBST cmp = isSorted . bstToList
  where
    isSorted [] = True
    isSorted [_] = True
    isSorted (x1 : x2 : xs) = (cmp x1 x2 == LT) && isSorted (x2 : xs)

-- | Searches given binary search tree for
-- given value with respect to given comparison
--
-- Returns found value (might not be the one that was given)
-- wrapped into 'Just' if it was found and 'Nothing' otherwise.
--
-- Usage example:
--
-- >>> tlookup compare 2 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Just 2
-- >>> tlookup compare 'a' Leaf
-- Nothing
-- >>> tlookup (\x y -> compare (x `mod` 3) (y `mod` 3)) 5 (Branch 2 (Branch 0 Leaf Leaf) (Branch 2 Leaf Leaf))
-- Just 2
tlookup :: Cmp a -> a -> Tree a -> Maybe a
tlookup _ _ Leaf = Nothing
tlookup cmp val (Branch a left right) = case cmp val a of
  LT -> tlookup cmp val left
  GT -> tlookup cmp val right
  EQ -> Just a

-- | Inserts given value into given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- If the same value with respect to comparison
-- was already present in the 'Tree' then replaces it with given value.
--
-- Usage example:
--
-- >>> tinsert compare 0 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 (Branch 0 Leaf Leaf) Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf)
-- >>> tinsert compare 'a' Leaf
-- Branch 'a' Leaf Leaf
tinsert :: Cmp a -> a -> Tree a -> Tree a
tinsert _ val Leaf = Branch val Leaf Leaf
tinsert cmp val (Branch a left right) = case cmp val a of
  LT -> Branch a (tinsert cmp val left) right
  GT -> Branch a left (tinsert cmp val right)
  EQ -> Branch val left right

-- | Deletes given value from given binary search tree
-- preserving its BST properties with respect to given comparison
--
-- Returns updated 'Tree' if the value was present in it;
-- or unchanged 'Tree' otherwise.
--
-- Usage example:
--
-- >>> tdelete compare 1 (Branch 2 (Branch 1 Leaf Leaf) (Branch 3 Leaf Leaf))
-- Branch 2 Leaf (Branch 3 Leaf Leaf)
-- >>> tdelete compare 'a' Leaf
-- Leaf
tdelete :: Cmp a -> a -> Tree a -> Tree a
tdelete _ _ Leaf = Leaf
tdelete cmp val (Branch a left right) = case cmp val a of
  LT -> Branch a (tdelete cmp val left) right
  GT -> Branch a left (tdelete cmp val right)
  EQ -> preserve (Branch a left right)
  where
    preserve Leaf = Leaf
    preserve (Branch _ Leaf Leaf) = Leaf
    preserve (Branch _ leftie rightie) = Branch (leftMost rightie) leftie (tdelete cmp (leftMost rightie) rightie)

leftMost :: Tree a -> a
leftMost = fromJust . leftMostMaybe

fromJust :: Maybe a -> a
fromJust Nothing = error "oopsie :)"
fromJust (Just a) = a

leftMostMaybe :: Tree a -> Maybe a
leftMostMaybe Leaf = Nothing
leftMostMaybe (Branch a Leaf _) = Just a
leftMostMaybe (Branch _ left _) = leftMostMaybe left
