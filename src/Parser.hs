{-# LANGUAGE LambdaCase #-}
module Parser where

import Data.List (isPrefixOf)

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

nop :: Parser ()
nop = Parser $ \s -> Just ((), s)

quit :: Parser a
quit = Parser (const Nothing)

phrase :: String -> Parser a -> Parser a
phrase match p = Parser $ \s ->
  if match `isPrefixOf` s
  then runParser (drop (length match) s) p
  else Nothing

eatTo :: Parser a -> Parser a
eatTo p =
  Parser $ \case
    ss@(_:rest) ->
      case parser p ss of
        Nothing -> parser (eatTo p) rest
        x -> x
    _ -> Nothing

toNext :: String -> Parser ()
toNext  s = eatTo (phrase s nop)

one :: Char -> Parser Char
one c = Parser $ \(s:rest) ->
  if s == c
  then Just (s, rest)
  else Nothing

many :: Char -> Parser String
many c = Parser $ \s ->
  let res = go c s
  in Just (res, drop (length res) s)
  where
    go :: Char -> String -> String
    go c (s:rest) =
      if s == c
        then s : go c rest
        else []

lookAhead :: Parser a -> Parser a
lookAhead p =
  Parser $ \s ->
    case runParser s p of
      Just (result, _) -> Just (result, s)
      Nothing -> Nothing

isnt :: Char -> Parser Char
isnt c = Parser $ \(s:rest) ->
  if c == s
  then Nothing
  else Just (s, rest)

repeatUntil :: Parser a -> Parser [a]
repeatUntil p = Parser $ \s -> Just $ go p s
  where
    go :: Parser a -> String -> ([a], String)
    go p s =
      case runParser s p of
        Just (res, rest) ->
          let (r, rr) = go p rest
           in (res : r, rr)
        Nothing -> ([], s)

takeUntil :: String -> Parser String
takeUntil str =
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
