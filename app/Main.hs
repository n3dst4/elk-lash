module Main (main) where

import           Control.Monad         (forever)
import           Data.Char             (isLower)
import           Lib                   (Alphabet, getAnagrams, getSubgrams,
                                        mkTree)
import           System.Console.GetOpt (ArgDescr (NoArg), ArgOrder (Permute),
                                        OptDescr (Option), getOpt)
import           System.Environment    (getArgs)
import           System.IO             (hFlush, stdout)

aToZ :: Alphabet
aToZ = ['a' .. 'z']

goodWord :: String -> Bool
goodWord = do
  len <- (>1) . length
  lc <- isLower . head
  return (len && lc)

-- cmd line option flags
data Flag = SubgramFlag | AnagramFlag deriving (Eq, Show)

-- option definitions
options :: [OptDescr Flag]
options =
  [ Option ['s'] ["subgram"] (NoArg SubgramFlag) "return subgrams instead of anagrams"
  , Option ['a'] ["anagram"] (NoArg AnagramFlag) "return anagrams (default)"
  ]

main :: IO ()
main = do
  text <- readFile "/usr/share/dict/words"
  let
    words = filter goodWord . lines $ text
    tree = mkTree aToZ words
  putStrLn $ "Finished reading dictionary (" ++ show (length words) ++ ")"
  hFlush stdout
  args <- getArgs
  let (opts, nonOpts, _) = getOpt Permute options args
      -- i know we only have these two but this is an exercise in futureproofing
      modeOpts = filter (`elem` [SubgramFlag, AnagramFlag]) opts
      (action, description) = case reverse modeOpts of
        SubgramFlag : _ -> (getSubgrams, "Subgrams")
        _               -> (getAnagrams, "Anagrams")
      text = concat nonOpts
      results = action aToZ text tree
  traverse putStrLn results
  hFlush stdout
