{-# LANGUAGE OverloadedStrings #-}
module LivingRoom (parse) where

import Data.Text
import Parser
import Types

data Period = Am | Pm

instance Show Period where
  show Am = "am" 
  show Pm = "pm"

data Time = Tm Integer Text Period

instance Show Time where
  show (Tm hr rest period) = show hr ++ unpack rest ++ show period 

time :: Date -> Parser Text
time d = do
  one_ '{'
  toNext "\"showtime_start\":\""
  phrase_ (pack $ show d)
  one_ ' '
  t1 <- takeUntil ":"
  t2 <- takeUntil ":"
  toNext "}"
  optional (one_ ',')
  let h = read $ unpack t1
  let (period, hour) = hp h
  pure . pack . show $ Tm hour t2 period
  where
    hp h
      | h == 12 = (Pm, h)
      | h > 12 = (Pm, h `mod` 12)
      | otherwise = (Am, h)

times :: Date -> Parser [Text]
times d = do
  one_ '['
  ts <- repeatUntil (time d)
  one_ ']'
  pure ts

film :: Date -> Parser Film
film d = do
  one_ '{'
  toNext "\"name\":\""
  title <- takeUntil "\""
  toNext "\"showtimes\":"
  ts <- times d
  one_ '}'
  optional (one_ ',')
  pure $ Film title ts

parse :: Date -> Parser [Film]
parse d = toNext "presetMovies = [" >> repeatUntil (film d)
