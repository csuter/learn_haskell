module Main where

import Test.Hspec
import Probs1to10

main :: IO ()
main = hspec $ do
  -- Problem 1
  describe "myLast" $ do
    it "returns the last entry in a list" $
      myLast [1, 2, 3, 4] `shouldBe` 4
    it "works for strings too" $
      myLast "xyz" `shouldBe` 'z'

  -- Problem 2
  describe "myButLast" $ do
    it "returns 2nd to last entry in a list" $
      myButLast [1, 2, 3, 4] `shouldBe` 3
    it "works for strings too" $
      myButLast ['a'..'z'] `shouldBe` 'y'

  -- Problem 3
  describe "elementAt" $ do
    it "returns the element at a given (1-indexed) position" $
      elementAt ['a', 'b', 'c', 'd', 'e'] 3 `shouldBe` 'c'
    it "works for strings too" $
      elementAt ['a'..'z'] 5 `shouldBe` 'e'

  -- Problem 4
  describe "myLength" $ do
    it "returns the length of a given list" $
      myLength ['a'..'z'] `shouldBe` 26

  -- Problem 5
  describe "myReverse" $ do
    it "returns a list in reversed order" $
      myReverse [1, 2, 3, 4] `shouldBe` [4, 3, 2, 1]

  -- Problem 6
  describe "isPalindrome" $ do
    it "returns True if the given string is a palindrome" $
      isPalindrome "asdfdsa" `shouldBe` True
    it "returns False if the given string is not a palindrome" $
      isPalindrome "asdf" `shouldBe` False

  -- Problem 7
  describe "flatten" $ do
    it "flattens a NestedList consisting of just an element" $
      flatten (Elem 5) `shouldBe` [5]
    it "flattens a more complicated NestedList" $
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1, 2, 3, 4, 5]

  -- Problem 8
  describe "compress" $ do
    it "removes consecutive duplicate elements from a list" $
      compress [1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5] `shouldBe` [1, 2, 3, 1, 4, 5]

  -- Problem 9
  describe "pack" $ do
    it "packs repeated list elements into sublists" $
      pack [1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5] `shouldBe` [[1, 1, 1, 1], [2], [3, 3], [1, 1], [4], [5, 5, 5, 5]]

  -- Problem 10
  describe "encode" $ do
    it "returns a run-length enocding of the given list" $
      (encode "aaaabccaadeeee") `shouldBe` [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')] 
