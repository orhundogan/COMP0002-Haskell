import qualified Data.Set as HS (fromList, toList)

data Set a = Set [a] deriving (Ord, Read, Show) 

-- toList uses quickSort
sort [] = []
sort (x:xs) = sort[a | a <- xs, a <= x] ++ [x] ++ sort[b | b <- xs, b > x]

toList :: Ord a => Set a -> [a]
toList (Set a) = sort [x|x<-a]

-- fromList uses duplicates

duplicates [] = []
duplicates (x:xs) = x: (duplicates (remove x xs))
    where
        remove x [] = []
        remove x (y:ys)
                |x==y = remove x ys
                | otherwise = y:(remove x ys)


fromList :: Ord a => [a] -> Set a
fromList [] = Set []
fromList xs = Set ys
              where ys = duplicates xs  


-- test if sets have same values
instance (Ord a) => Eq (Set a) where
    s1 == s2 | toList s1 == toList s2 = True
             | otherwise = False

-- empty set
empty :: Set a
empty = Set []

-- is it the empty set?
--isNull :: Set a -> Bool
--isNull xs | length' xs == 0 = True
--          | otherwise = False

-- build a one element Set
singleton :: a -> Set a
singleton x = Set [x]

-- insert an element *x* of type *a* into Set *s*. Make sure there are no duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert x (Set xs) | x `elem` xs = Set xs
                  | otherwise   = Set (x:xs)

-- join two Sets together be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (xs++[y|y<-ys,y `notElem` xs])

-- return, as a Set, the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection (Set xs) (Set ys) = Set [x|x<-xs,x `elem` ys]

-- all the elements in *s1* not in *s2*
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference   (Set xs) (Set ys) = Set [x|x<-xs,x `notElem` ys]

-- is element *x* in the Set s1?
member :: (Ord a) => a -> Set a -> Bool
member x (Set xs) = x `elem` xs

-- how many elements are there in the Set?
--cardinality :: Set a -> Int
--cardinality (Set xs) = amount (toList xs)

-- apply a function to every element in the Set
--setmap :: (Ord b) => (a -> b) -> Set a -> Set b
-- setmap f s = undefined

-- right fold a Set using a function *f*
--setfoldr :: (a -> b -> b) -> b -> Set a -> b
--setfoldr f acc s = undefined

-- remove an element *x* from the set
-- return the set unaltered if *x* is not present
--removeSet :: (Eq a) => a -> Set a -> Set a
--removeSet x s = fromList(removeItem x (toList s))


removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerset :: Set a -> Set (Set a)
powerset (Set xs) = fromList (power (toList ys))
                    where ys = toList (Set xs)

power :: [a] -> [[a]]
power [] = [[]]
power (x:xs) = map (x:) (power xs) ++ power xs 
