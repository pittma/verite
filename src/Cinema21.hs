{-# LANGUAGE OverloadedStrings #-}
module Cinema21 (parse) where

import Prelude hiding (repeat)

import Data.Text
import Data.Text.Encoding (decodeLatin1)
import Types (Date(..), Film(..))
import Text.Parselet

time :: Parser Text
time = do
  one '{'
  toNext "'time': '"
  time <- takeUntil "'"
  toNext "}"
  phrase ",\n"
  whitespace
  pure time

times :: Parser [Text]
times = do
  toNext "'times': [\n"
  whitespace
  ts <- repeatUntil time
  phrase "]\n"
  pure ts

film :: Parser Film
film = do
  one '{'
  toNext "'title': '"
  title <- takeUntil "'"
  ts <- times
  whitespace
  phrase "},\n"
  whitespace
  pure (Film title ts)

parse :: Date -> Parser [Film]
parse today = do
  toNext ("'" <> pack (show today) <> "': [\n")
  toNext "*/\n"
  whitespace
  repeatUntil film
