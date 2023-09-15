module Main (main) where

import Data.Functor ((<&>))
import Data.Time.Clock
import Data.Time.Calendar
import Types
import qualified Cinema21 as C21

date :: IO Date
date = do
  (year, month, day) <- getCurrentTime <&> (toGregorian . utctDay)
  return (Date year month day)

filmPrinter :: Film -> IO ()
filmPrinter (Film title times) = do
  putStrLn ("  " ++ title)
  mapM_ stimes times
  where
    stimes t = putStrLn ("    " ++ t)

runFetcher :: String -> Date -> (Date -> IO (Maybe [Film])) -> IO ()
runFetcher name d f = do
  r <- f d
  case r of
    Just films -> do
      putStrLn name
      mapM_ filmPrinter films
    Nothing -> putStrLn ("failed to retrieve " ++ name ++ " showtimes")
  
main :: IO ()
main = do
  today <- date
  runFetcher "Cinema 21" today C21.fetch
