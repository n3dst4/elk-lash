{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

--import           Control.Lens               ((^.))
import           Control.Monad              (forever)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Char                  (isLower)
import           Lib                        (Alphabet, getAnagrams,
                                             getAnagramsOptimized, getSubgrams,
                                             mkTree)
--import qualified Network.Wreq               as Wr
import           System.Console.GetOpt      (ArgDescr (NoArg),
                                             ArgOrder (Permute),
                                             OptDescr (Option), getOpt)
import           System.Environment         (getArgs)
import           System.IO                  (hFlush, stdout)
aToZ :: Alphabet
aToZ = ['a' .. 'z']

dictUrl :: String
dictUrl = "https://gist.githubusercontent.com/n3dst4/ac7ca7fa851a396e4269d68aaaa6ed58/raw/b1f64ca00c94a4265d66d91940a658bd705e556f/words"
dictFile :: String
dictFile = "./british-english-small"

goodWord :: String -> Bool
goodWord = do
  len <- (>1) . length
  lc <- isLower . head
  return (len && lc)

-- cmd line option flags
data Flag = SubgramFlag | AnagramFlag | OptimizedFlag deriving (Eq, Show)

-- option definitions
options :: [OptDescr Flag]
options =
  [ Option ['s'] ["subgram"] (NoArg SubgramFlag) "return subgrams instead of anagrams"
  , Option ['a'] ["anagram"] (NoArg AnagramFlag) "return anagrams (default)"
  , Option ['o'] ["anagram"] (NoArg OptimizedFlag) "return anagrams (optimized)"
  ]

main :: IO ()
main = do
  text <- readFile dictFile
  --r <- Wr.get dictUrl
  let
    --textBS = r ^. Wr.responseBody
    --text = unpack textBS
    words = filter goodWord . lines $ text
    tree = mkTree aToZ words
  putStrLn $ "Finished reading dictionary (" ++ show (length words) ++ ")"
  hFlush stdout
  args <- getArgs
  let (opts, nonOpts, _) = getOpt Permute options args
      -- i know we only have these two but this is an exercise in futureproofing
      modeOpts = filter (`elem` [SubgramFlag, AnagramFlag, OptimizedFlag]) opts
      (action, description) = case reverse modeOpts of
        SubgramFlag : _   -> (getSubgrams, "Subgrams")
        OptimizedFlag : _ -> (getAnagramsOptimized, "Optimized")
        _                 -> (getAnagrams, "Anagrams")
      text = concat nonOpts
      results = action aToZ text tree
  traverse putStrLn results
  hFlush stdout
