module TestUtil
( expectEq
) where

expectEq :: (Eq a, Show a) => a -> a -> Bool
expectEq x y
    | x == y = True
    | x /= y = error (
        "Expected equal values, got " ++ (show x) ++ " and " ++ (show y))
