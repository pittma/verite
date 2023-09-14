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

eatUntil :: Parser a -> Parser a
eatUntil p =
  Parser $ \case
    ss@(_:rest) ->
      case parser p ss of
        Nothing -> parser (eatUntil p) rest
        x -> x
    _ -> Nothing

keepUntil :: String -> Parser String
keepUntil str =
  Parser $ \s ->
    case go str s of
      ([], []) -> Nothing
      result -> Just result
  where
    go :: String -> String -> (String, String)
    go s (ss:str2) =
      if s `isPrefixOf` str2
        then ([ss], str2)
        else let (result, rest) = go s str2
              in (ss : result, rest)
    go _ [] = ([], [])
        

data Film =
  Film String [String]
  deriving (Show)
      

series :: String -> Parser a -> Parser a
series match p = Parser $ \s ->
  if match `isPrefixOf` s
  then runParser (drop (length match) s) p
  else Nothing

nop :: Parser ()
nop = Parser $ \s -> Just ((), s)

movieData :: Parser ()
movieData = eatUntil $ series "movieData = " nop

today :: String -> Parser ()
today s = eatUntil $ series (('\'' : s) ++ "': [") nop

keep :: Parser String
keep = Parser $ \s -> Just (s, mempty)

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) a b = Parser $ \s -> case runParser s a of
  Nothing -> runParser s b
  x -> x

time :: Parser String
time = do
  eatUntil $ series "'time': '" nop
  keepUntil "'"

-- | recurse through the stream looking for `match`, repeatedly
-- | applying `p` along the way.
foldUntil :: String -> [a] -> Parser a -> Parser [a]
foldUntil match acc p = Parser $ \s -> case go acc s p of
  ([], mempty) -> Nothing
  (res, rest) -> Just (res, rest)
  where
    go :: [a] -> String -> Parser a -> ([a], String)
    go acc s p = if match `isPrefixOf` s
      then (acc, s)
      else case runParser s p of
             Just (result, rest) -> (result : fst (go acc rest p), rest)
             Nothing -> ([], mempty)
  

parse :: String -> Maybe (Film, String)
parse str = runParser str $ do
  movieData
  today "2023-09-14"
  eatUntil $ series "title': '" nop
  title <- keepUntil "'"
  times <- foldUntil " ]" [] time
  return (Film title times)

getC21 :: IO ()
getC21 = runReq defaultHttpConfig $ do
  bs <- req GET (https "cinema21.com") NoReqBody bsResponse mempty
  liftIO $ case parse $ B.unpack (responseBody bs) of
    Just (json, out) ->
      print json
    Nothing -> print "didn't work"
