{-# LANGUAGE OverloadedStrings #-}
module Laurelhurst where

import Data.Text
import Data.Text.Encoding (decodeLatin1)
import Parser
import Types

time :: Parser Text
time = do
  one_ '{'
  toNext "\"timeStr\":\""
  time <- takeUntil "\""
  upto '}'
  optional (phrase_ "},")
  return time

times :: Date -> Parser [Text]
times d = do
  one_ '{'
  phrase_ ("\"" <> pack (dateStr d "") <> "\":[")
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
  
parse ::  Date -> Text -> Maybe [Film]
parse d s = fmap fst $ runParser s $ do
  toNext "var gbl_movies = {"
  repeatUntil (film d)
