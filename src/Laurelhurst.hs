{-# LANGUAGE OverloadedStrings #-}
module Laurelhurst where

import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

import Parser
import Types

time :: Parser String
time = do
  one_ '{'
  toNext "\"timeStr\":\""
  time <- takeUntil "\""
  upto '}'
  optional (phrase_ "},")
  return time


times :: Date -> Parser [String]
times d = do
  one_ '{'
  phrase_ ("\"" ++ dateStr d "" ++ "\":[")
  ts <- repeatUntil time
  toNext "}]}"
  return ts

film :: Date -> Parser Film
film d = do
  one_ '"'
  toNext "\":"
  one_ '{'
  toNext "\"title\":\""
  title <- takeUntil "\""
  toNext "\"schedule\":"
  ts <- times d
  one_ '}'
  one_ ','
  return (Film title ts)
  
parse ::  Date -> String -> Maybe [Film]
parse d s = fmap fst $ runParser s $ do
  toNext "var gbl_movies = {"
  repeatUntil (film d)

fetch :: Date -> IO (Maybe [Film])
fetch today =
  runReq defaultHttpConfig $ do
    bs <- req GET (https "laurelhursttheater.com") NoReqBody bsResponse mempty
    return (parse today $ B.unpack (responseBody bs))
