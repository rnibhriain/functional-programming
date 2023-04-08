{- butrfeld Andrew Butterfield -}
module Ex04 where

name, idno, username :: String
name      =  "Róisín Ní Bhriain"  -- replace with your name
idno      =  "18326577"    -- replace with your student id
username  =  "rnibhria"   -- replace with your TCD username

declaration -- do not modify this
 = unlines
     [ ""
     , "@@@ This exercise is all my own work."
     , "@@@ Signed: " ++ name
     , "@@@ "++idno++" "++username
     ]

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype, honestly!
data BinTree k d
  = Branch (BinTree k d) (BinTree k d) k d
  | Leaf k d
  | Empty
  deriving (Eq, Show)


-- Part 1 : Tree Insert -------------------------------

-- Implement:
ins :: Ord k => k -> d -> BinTree k d -> BinTree k d
ins x y Empty = Leaf x y
ins x y (Leaf k d) 
       | x < k  = Branch (Leaf x y) Empty k d
       | x > k  = Branch Empty (Leaf x y) k d
       | x == k = Leaf k y
ins x y (Branch left right k d)
       | x < k  = Branch (ins x y left) right k d
       | x > k  = Branch left (ins x y right) k d
       | x == k = Branch left right k y
ins _ _ _  = error "ins NYI"

-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => BinTree k d -> k -> m d
lkp Empty key =  fail "lkp NYI"
lkp (Leaf k d) key
       | k == key = return d
       | otherwise = fail "lkp NYI"
lkp (Branch left right k d) key
       | k > key  = lkp left key
       | k < key  = lkp right key
       | k == key = return d

-- Part 3 : Tail-Recursive Statistics

{-
   It is possible to compute BOTH average and standard deviation
   in one pass along a list of data items by summing both the data
   and the square of the data.
-}
twobirdsonestone :: Double -> Double -> Int -> (Double, Double)
twobirdsonestone listsum sumofsquares len
 = (average,sqrt variance)
 where
   nd = fromInteger $ toInteger len
   average = listsum / nd
   variance = sumofsquares / nd - average * average

{-
  The following function takes a list of numbers  (Double)
  and returns a triple containing
   the length of the list (Int)
   the sum of the numbers (Double)
   the sum of the squares of the numbers (Double)

   You will need to update the definitions of init1, init2 and init3 here.
-}
getLengthAndSums :: [Double] -> (Int,Double,Double)
getLengthAndSums ds 
 = getLASs init1 init2 init3 ds 
 where 
  init1 = 0
  init2 = 0
  init3 = 0
    


{-
  Implement the following tail-recursive  helper function
-}
getLASs :: Int -> Double -> Double -> [Double] -> (Int,Double,Double)
getLASs init1 init2 init3 [] = (init1, init2, init3)
getLASs init1 init2 init3 (d:ds)
  = getLASs ln sm sq ds 
  where
    ln = init1 + 1
    sm = init2 + d
    sq = init3 + d * d


-- Final Hint: how would you use a while loop to do this?
--   (assuming that the [Double] was an array of double)
