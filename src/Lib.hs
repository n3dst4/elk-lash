{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}

module Lib
    ( addWord
    , getAnagrams
    , getAnagramsOptimized
    , getSubgrams
    , mkHisto
    , mkTree
    , normalize
    , upsert
    , Alphabet
    , SubgramTree (..)
    , SubgramTreeException (..)
    ) where

import           Control.Exception (Exception, throw)
import           Data.Char         (toLower)
import           Data.Foldable     (fold)
import           Data.List         (sort, span)
import           Data.Sequence     (Seq, adjust, empty,
                                    replicate, take, (><), (|>))
import qualified Data.Sequence     as S (filter, zip)
import           Data.Text         ()
import           Prelude           hiding (replicate, take)

-- TYPES

type Alphabet = String

data SubgramTree = Node (Seq SubgramTree)
                 | Leaf (Seq String)
                   deriving (Eq, Show)

type Histogram = [Int]

data SubgramTreeException =
  HistogramOutOfBounds | HistogramUnderrun deriving (Show)

instance Exception SubgramTreeException


-- FUNCTIONS

normalize :: Alphabet -> String -> String
normalize alphabet = filter (`elem` alphabet) . map toLower

-- | Given an alphabet, turn a string into a list of counts of the occurrrences
-- of each letter of the alphabet. E.g. if the alphabet is "abcde" and the word
-- is "decade", the histogram is [1, 0, 1, 2, 2] (one 'a', zero 'b's etc.)
mkHisto :: Alphabet -> String -> Histogram
mkHisto alphabet word = mkHisto' alphabet (sort $ normalize alphabet word)
  where
    mkHisto' [] _ = []
    mkHisto' (ltr : alphabet') puddle =
      let (ltrs, puddle') = span (== ltr) puddle
          ltrCount = length ltrs
      in ltrCount : mkHisto' alphabet' puddle'


-- | update a Seq in place, padding with given "empty" values if needed
-- (usual Seq adjust behaviour is to ignore updates if index is oob)
upsert :: a -> Int -> (a -> a) -> Seq a -> Seq a
upsert z i f xs =
  if i >= length xs then
    let padding = replicate (i - length xs) z
    in (xs >< padding) |> f z
  else
    adjust f i xs



-- TREE STUFFS

-- | add a word to a subgram tree
addWord :: Alphabet -> String -> SubgramTree -> SubgramTree
addWord alphabet word = go $ mkHisto alphabet word
  where
    go :: Histogram -> SubgramTree -> SubgramTree
    go [] (Node _) =  Leaf $ pure word
    go [] (Leaf xs) = Leaf $ xs |> word
    go (freq : histo') (Node kids) = Node $
      upsert (Node empty) freq (go histo') kids
    go (_ : _) (Leaf _) = throw HistogramOutOfBounds


-- | create a new tree from the given list of words
mkTree :: Alphabet -> [String] -> SubgramTree
mkTree alphabet = foldr (addWord alphabet) (Node empty)


-- | find all the subgrams of a word from a tree
getSubgrams :: Alphabet -> String -> SubgramTree -> Seq String
getSubgrams alphabet haystack = getSubgrams' empty $ mkHisto alphabet haystack

remove :: Eq a => Seq a -> Seq a -> Seq a
remove xs = S.filter (not . (`elem` xs))

getSubgrams' :: Seq String -> Histogram -> SubgramTree -> Seq String
getSubgrams' _ (_ : _) (Leaf _)      = throw HistogramOutOfBounds
getSubgrams' _ [] (Node _)           = empty
getSubgrams' excl [] (Leaf ws)       = remove excl ws
getSubgrams' excl (i : is) (Node trees) =
  fold $ fmap (getSubgrams' excl is) (take (i + 1) trees)

getAnagrams :: Alphabet -> String -> SubgramTree -> Seq String
getAnagrams alphabet haystack tree = getAnagrams' $ mkHisto alphabet haystack
  where
    getAnagrams' :: Histogram -> Seq String
    getAnagrams' histo =
      let
        subgrams = getSubgrams' empty histo tree
        go :: String -> Seq String
        go word =
          if all (== 0) histo'
            then pure word
            else fmap ((word ++ " ") ++) (getAnagrams' histo')
          where
            histo' = zipWith subtract (mkHisto alphabet word) histo
      in
        subgrams >>= go

getAnagramsOptimized :: Alphabet -> String -> SubgramTree -> Seq String
getAnagramsOptimized alphabet haystack tree = getAnagrams' [] $ mkHisto alphabet haystack
  where
    stackSeq :: Seq a -> Seq (a, Seq a)
    stackSeq xs =
      let
        indexed = S.zip [0 .. length xs] xs
        f (i, x) = (x, take i xs)
      in fmap f indexed
    getAnagrams' :: Seq String -> Histogram -> Seq String
    getAnagrams' excl histo =
      let
        subgrams = stackSeq $ getSubgrams' excl histo tree
        go :: (String, Seq String) -> Seq String
        go (word, excl') =
          if all (== 0) histo'
            then pure word
            else fmap ((word ++ " ") ++) (getAnagrams' (excl >< excl') histo')
          where
            histo' = zipWith subtract (mkHisto alphabet word) histo
      in
        subgrams >>= go
