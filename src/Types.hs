module Types where

data Film = Film
  { f_title :: String
  , f_showtimes :: [String]
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
