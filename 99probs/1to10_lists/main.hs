import TestUtil
import Problems

tests = [
    -- Problem 1
    expectEq 4 (myLast [1, 2, 3, 4]),
    expectEq 'z' (myLast ['x', 'y', 'z']),

    -- Problem 2
    expectEq 3 (myButLast [1, 2, 3, 4]),
    expectEq 'y' (myButLast ['a'..'z']),

    -- Problem 3
    expectEq 'c' (elementAt ['a', 'b', 'c', 'd', 'e'] 3),
    expectEq 'e' (elementAt ['a'..'z'] 5),

    -- Problem 4
    expectEq 26 (myLength ['a'..'z']),

    -- Problem 5
    expectEq [4, 3, 2, 1] (myReverse [1, 2, 3, 4]),

    -- Problem 6
    expectEq True (isPalindrome "asdfdsa"),
    expectEq False (isPalindrome "asdf"),

    -- Problem 7
    expectEq [5] (flatten (Elem 5)),
    expectEq [1, 2, 3, 4, 5] (flatten (
        List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])),

    -- Problem 8
    expectEq [1, 2, 3, 1, 4, 5] (compress [
        1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5]),

    -- Problem 9
    expectEq [[1, 1, 1, 1], [2], [3, 3], [1, 1], [4], [5, 5, 5, 5]] (
        pack [1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5]),

    -- Problem 10
    expectEq [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')] (
        encode "aaaabccaadeeee")
    ]

main = do
    results <- sequence (map print tests)
    print results
