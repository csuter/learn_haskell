module Probs1to10
( myLast
, myButLast
, elementAt
, myLength
, myReverse
, isPalindrome
, NestedList(..)
, flatten
, compress
, pack
, encode
) where

-- Problem 1
-- (*) Find the last element of a list
myLast :: [a] -> a
myLast [] = error "Can't call myLast on an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
-- (*) Find the last but one element of a list
myButLast :: [a] -> a
myButLast [] = error "Can't call myButLast on empty list"
myButLast [x] = error "Can't call myButLast on list with length 1"
myButLast [x, y] = x
myButLast (_:xs) = myButLast xs

-- Problem 3
-- (*) Find th K'th element of a list. The first element in the list is number 1
elementAt :: [a] -> Int -> a
elementAt [] n = error "Empty list"
elementAt (x:xs) 1 = x
elementAt (x:xs) n
    | length xs < n = error "Index out of bounds"
    | otherwise = elementAt xs (n - 1)

-- Problem 4
-- (*) Find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
-- (*) Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Problem 6
-- (*) Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = and $ zipWith (==) x (myReverse x)

-- Problem 7
-- Transform a list, possibly holding lists as elements, into a 'flat' list by
-- replacing each list with its elements (recursively)
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:zs)
    | (x == y) = compress (x:zs)
    | (x /= y) = x:(compress (y:zs))

-- Problem 9
-- (**) Pack consecutive duplicates of a list into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:y:zs)
    | (x == y) = [[x] ++ p] ++ ps
    | (x /= y) = [[x]] ++ (p:ps)
    where p:ps = pack (y:zs)

-- Problem 10
-- (*) Run-length encoding of a list.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, elementAt x 1)) . pack
