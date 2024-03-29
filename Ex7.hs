{- Exercise 7, due (Nov 19, 11:50pm)

General exercise instructions:
- Exercises must be done *individually*.
- You may not import any Haskell libraries, unless explicitly told to.
- You may write helper functions freely; in fact, you are encouraged
  to do so to keep your code easy to understand.
- Your grade will be determined by our automated testing.
  You can find some sample tests on the course webpage.
- Submit early and often! MarkUs is rather slow when many people
  submit at once. It is your responsibility to make sure your work is
  submitted on time.
- No late submissions will be accepted!
-}

-- This line creates a module to allow exporting of functions.
-- DON'T CHANGE IT!
module Ex7 (domain, TripleDeep(ShallowEnd, DeepEnd)) where

-- Question 1
-- An association list is a list of 2-tuples.
-- For example, [("temp", 34), ("height",80), ("weight", 180), ("depth", 7)].
-- Define a function domain :: Eq a â‡’ [(a, b)] â†’ [a]
-- which takes an association list and returns the list of all those
-- things that occur in the first component of each tuple. Make sure that the value
-- of domain does not contain any duplicates.
-- For example, domain [(1, 2), (2, 3), (1, 3)] = [1,2]
domain :: Eq a => [(a, b)] -> [a]
domain [] = []
domain a = foldl (\x y -> if (elem (fst y) x) then x else x ++ [(fst y)]) [] a


-- TripleDeep is a datatype to define a tree, where each node contains either:
--   - A value of some type a
--   - Three TripleDeep subtrees
-- Write a Functor instance for TripleDeep.
data TripleDeep a =
      ShallowEnd a
    | DeepEnd (TripleDeep a) (TripleDeep a) (TripleDeep a)
    deriving(Show, Eq)

-- An example of a TripleDeep structure.
tree :: TripleDeep Integer
tree = DeepEnd (ShallowEnd 1)
               (DeepEnd (ShallowEnd 5) (ShallowEnd 2) (ShallowEnd 3))
               (DeepEnd (ShallowEnd 1) (ShallowEnd 2) (ShallowEnd 3))
-- Question 2
-- Write a Functor instance for TripleDeep.
instance Functor TripleDeep where
    fmap f (DeepEnd x b c) = (DeepEnd
                               (fmap f x) (fmap f b) (fmap f c)
                             )
    fmap f (ShallowEnd x) = (ShallowEnd (f x))
