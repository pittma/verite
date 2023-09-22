module Types where

import Data.Text
import Parser (Parser)

data TheaterParser = TP
  { tp_parser :: Date -> Parser [Film]
  , tp_name :: String
  , tp_hostname :: String
  }
  
data Film = Film
  { f_title :: Text
  , f_showtimes :: [Text]
  } deriving (Show)

data Date = Date Integer Int Int

dateStr :: Date -> String -> String
dateStr (Date year month day) c = 
    show year ++ c ++ leadZero month ++ c ++ leadZero day
    where
      leadZero i =
        if i < 10
          then "0" ++ show i
          else show i

instance Show Date where
  show d = dateStr d "-"
