{-# LANGUAGE OverloadedStrings #-}
module Cinema21 (parse) where

import Data.Text
import Data.Text.Encoding (decodeLatin1)
import Types
import Parser

time :: Parser Text
time = do
  one '{'
  toNext "'time': '"
  time <- takeUntil "'"
  toNext "}"
  phrase ",\n" nop
  many ' '
  pure time

times :: Parser [Text]
times = do
  toNext "'times': [\n"
  many ' '
  ts <- repeatUntil time
  phrase "]\n" nop
  pure ts

film :: Parser Film
film = do
  one '{'
  toNext "'title': '"
  title <- takeUntil "'"
  ts <- times
  many ' '
  phrase "},\n" nop
  many ' '
  pure (Film title ts)

parse :: Date -> Parser [Film]
parse today = do
  toNext ("'" <> pack (show today) <> "': [\n")
  toNext "*/\n"
  many ' '
  repeatUntil film
