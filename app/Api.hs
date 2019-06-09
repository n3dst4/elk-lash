{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
  ( main
  )
where

import           Data.Aeson                     ( FromJSON
                                                , toJSON
                                                )
import           Data.Monoid                    ( mconcat )
import           Data.Char                      ( isLower )
import           Data.Sequence                  ( Seq )
import           Data.Foldable                  ( toList )
import           Data.List                      ( intercalate )
import           GHC.Generics                   ( Generic )
import           Lib                            ( Alphabet
                                                , getAnagrams
                                                , getSubgrams
                                                , mkTree
                                                )
import           Web.Scotty                     ( ActionM(..)
                                                , get
                                                , html
                                                , jsonData
                                                , json
                                                , post
                                                , scotty
                                                , param
                                                )
import           Data.Text.Lazy                 ( pack )


newtype Request = Request { haystack :: String } deriving (Generic, Show)

instance FromJSON Request

aToZ :: Alphabet
aToZ = ['a' .. 'z']


dictFile :: String
dictFile = "./dicts/british-english-small"

goodWord :: String -> Bool
goodWord = do
  len <- (> 1) . length
  lc  <- isLower . head
  return (len && lc)


main = do
  text <- readFile dictFile
  let words = (filter goodWord) $ lines $ text
      tree  = mkTree aToZ words
  putStrLn $ "Finished reading dictionary (" ++ show (length words) ++ ")"

  scotty 3000 $ do
    get "/" $ html $ pack
      ("<html>" ++
        "<h1>Subgrams service</h1>" ++ 
        "<p>GET /subgrams/<word or phrase> to get subgrams</p>" ++
        "</html>")
    get "/subgrams/:word" $ do
      word <- param "word"
      let subs     = getSubgrams aToZ word tree
      json subs
    post "/subgrams" $ do
      r <- jsonData :: ActionM Request
      html "<h1>not implemented</h1>"
