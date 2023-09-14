{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Lib where
import Control.Monad.IO.Class
import Data.List (isPrefixOf)
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B

  
newtype Parser a = Parser
  { parser :: String -> Maybe (a, String)
  }

runParser :: String -> Parser a -> Maybe (a, String)
runParser s p = parser p s

instance Functor Parser where
  fmap f p =
    Parser $ \s ->
      case runParser s p of
        Nothing -> Nothing
        Just (result, rest) -> Just (f result, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  pf <*> pa =
    Parser $ \s ->
      case runParser s pf of
        Nothing -> Nothing
        Just (f, rest) ->
          case runParser rest pa of
            Nothing -> Nothing
            Just (x, rest') -> Just (f x, rest')

instance Monad Parser where
  return = pure
  pa >>= f =
    Parser $ \s ->
      case runParser s pa of
        Nothing -> Nothing
        Just (result, rest) -> runParser rest (f result)

parse :: String -> Maybe String
parse s = undefined

data Film =
  Film String [String]
  deriving (Show)

getC21 :: IO ()
getC21 =
  runReq defaultHttpConfig $ do
    bs <- req GET (https "cinema21.com") NoReqBody bsResponse mempty
    liftIO
      $ case parse $ B.unpack (responseBody bs) of
          Just json -> print json
          Nothing -> print "didn't work"
