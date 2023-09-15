module Types where

data Film = Film
  { f_title :: String
  , f_showtimes :: [String]
  } deriving (Show)

data Date = Date Integer Int Int

instance Show Date where
  show (Date year month day) =
    show year ++ "-" ++ leadZero month ++ "-" ++ leadZero day
    where
      leadZero i =
        if i < 10
          then "0" ++ show i
          else show i
