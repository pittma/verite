{-# LANGUAGE OverloadedStrings #-}
module Cinema21 (fetch) where

import Control.Monad.IO.Class
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

import Types
import Parser

time :: Parser String
time = do
  one '{'
  toNext "'time': '"
  time <- takeUntil "'"
  toNext "}"
  phrase ",\n" nop
  many ' '
  return time

times :: Parser [String]
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

parse :: Date -> String -> Maybe [Film]
parse today s = fmap fst $ runParser s $ do
  toNext ("'" ++ show today ++ "': [\n")
  toNext "*/\n"
  many ' '
  repeatUntil film

fetch :: Date -> IO (Maybe [Film])
fetch today =
  runReq defaultHttpConfig $ do
    bs <- req GET (https "cinema21.com") NoReqBody bsResponse mempty
    return (parse today $ B.unpack (responseBody bs))
