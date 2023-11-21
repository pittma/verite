{-# LANGUAGE OverloadedStrings #-}
module Laurelhurst where

import Data.Text
import Data.Text.Encoding (decodeLatin1)
import Text.Parselet
import Types

time :: Parser Text
time = do
  one_ '{'
  toNext "\"timeStr\":\""
  time <- takeUntil "\""
  upto '}'
  optional (phrase_ "},")
  pure time

times :: Date -> Parser [Text]
times d = do
  one_ '{'
  phrase_ ("\"" <> pack (dateStr d "") <> "\":[")
  ts <- repeatUntil time
  toNext "}]}"
  pure ts

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
  pure (Film title ts)
  
parse ::  Date -> Parser [Film]
parse d = do
  toNext "var gbl_movies = {"
  repeatUntil (film d)
