{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Api (main) where

import           Data.Aeson   (FromJSON)
import           Data.Monoid  (mconcat)
import           GHC.Generics (Generic)
import           Lib          (Alphabet, getAnagrams, getSubgrams, mkTree)
import           Web.Scotty   (ActionM (..), get, html, jsonData, post, scotty)

newtype Request = Request { haystack :: String } deriving (Generic, Show)

instance FromJSON Request


main = scotty 3000 $ do
  get "/" $
    html "<h1>post {haystack: \"haystack\"} to /subgrams to get subgrams</h1>"
  get "/subgrams" $ do
    r <- (jsonData :: ActionM Request)
    html "<h1>not implemented</h1>"
