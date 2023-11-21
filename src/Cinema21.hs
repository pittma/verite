{-# LANGUAGE OverloadedStrings #-}
module Cinema21 (parse) where

import Prelude hiding (repeat)

import Data.Text
import Data.Text.Encoding (decodeLatin1)
import Types
import Text.Parselet

time :: Parser Text
time = do
  one '{'
  toNext "'time': '"
  time <- takeUntil "'"
  toNext "}"
  phrase ",\n"
  repeat ' '
  pure time

times :: Parser [Text]
times = do
  toNext "'times': [\n"
  repeat ' '
  ts <- repeatUntil time
  phrase "]\n"
  pure ts

film :: Parser Film
film = do
  one '{'
  toNext "'title': '"
  title <- takeUntil "'"
  ts <- times
  repeat ' '
  phrase "},\n"
  repeat ' '
  pure (Film title ts)

parse :: Date -> Parser [Film]
parse today = do
  toNext ("'" <> pack (show today) <> "': [\n")
  toNext "*/\n"
  repeat ' '
  repeatUntil film
