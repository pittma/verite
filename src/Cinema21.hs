{-# LANGUAGE OverloadedStrings #-}
module Cinema21 (fetch) where

import Control.Monad.IO.Class
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

import Types (Film)
import Parser

parse :: String -> Maybe [Film]
parse s = Nothing

fetch :: IO ()
fetch =
  runReq defaultHttpConfig $ do
    bs <- req GET (https "cinema21.com") NoReqBody bsResponse mempty
    liftIO
      $ case parse $ B.unpack (responseBody bs) of
          Just films -> print films
          Nothing -> print "didn't work"
