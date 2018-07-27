module Main where

import           Criterion.Main (bench, defaultMain, nf, whnf)
import           Data.Char      (isLower)
import           Lib            (Alphabet, getAnagrams, getAnagramsOptimized,
                                 mkTree)

dictFile :: String
dictFile = "./british-english-small"

goodWord :: String -> Bool
goodWord = do
  len <- (>1) . length
  lc <- isLower . head
  return (len && lc)

aToZ :: Alphabet
aToZ = ['a' .. 'z']

main :: IO ()
main = do
  text <- readFile dictFile
  let
    words = filter goodWord . lines $ text
    tree = mkTree aToZ words
  defaultMain
    [ bench "standard algo" $ nf (getAnagrams aToZ "londonzoospa" ) tree
    , bench "optimized algo" $ nf (getAnagramsOptimized aToZ "londonzoospa" ) tree
    ]
