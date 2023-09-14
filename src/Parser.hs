module Parser where

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
