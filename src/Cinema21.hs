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
  return time

times :: Parser [Text]
times = do
  toNext "'times': [\n"
  many ' '
  ts <- repeatUntil time
  phrase "]\n" nop
  return ts

film :: Parser Film
film = do
  one '{'
  toNext "'title': '"
  title <- takeUntil "'"
  ts <- times
  many ' '
  phrase "},\n" nop
  many ' '
  return (Film title ts)

parse :: Date -> Text -> Maybe [Film]
parse today s = fmap fst $ runParser s $ do
  toNext ("'" <> pack (show today) <> "': [\n")
  toNext "*/\n"
  many ' '
  repeatUntil film
