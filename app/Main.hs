module Main where

import           Control.Monad (forever)
import           Data.Char     (isLower)
import           Lib           (Alphabet, getAnagrams, getSubgrams, mkTree)
import           System.IO     (hFlush, stdout)

aToZ :: Alphabet
aToZ = ['a' .. 'z']

goodWord :: String -> Bool
goodWord = do
  len <- (>1) . length
  lc <- isLower . head
  return (len && lc)

main :: IO ()
main = do
  text <- readFile "/usr/share/dict/words"
  let
    words = filter goodWord . lines $ text
    tree = mkTree aToZ words
  putStrLn $ "Finished reading dictionary (" ++ (show $ length words) ++ ")"
  hFlush stdout
  forever $ do
    putStr "Enter your string: "
    hFlush stdout
    input <- getLine
    let anagrams = getSubgrams aToZ input tree
    putStrLn "Subgrams: "
    print anagrams
    hFlush stdout
