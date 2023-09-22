module Main (main) where

import Data.Functor ((<&>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeLatin1)
import Data.Time.Clock
import Data.Time.Calendar
import Network.HTTP.Req
import Parser (Parser, runParser)
import Types
import qualified Cinema21 as C21
import qualified Laurelhurst as L
import qualified LivingRoom as LL

theaters :: [TheaterParser]
theaters =
  [ TP
      { tp_name = "Cinema 21"
      , tp_hostname = "cinema21.com"
      , tp_parser = C21.parse
      }
  , TP
      { tp_name = "Laurelhurst Theater"
      , tp_hostname = "laurelhursttheater.com"
      , tp_parser = L.parse
      }
  , TP
      { tp_name = "Living Room Theater"
      , tp_hostname = "pdx.livingroomtheaters.com"
      , tp_parser = LL.parse
      }
  ]
  
date :: IO Date
date = do
  (year, month, day) <- getCurrentTime <&> (toGregorian . utctDay)
  pure (Date year month day)

parse :: Parser [Film] -> Text -> Maybe [Film]
parse p s = fst <$> runParser s p

fetch :: Date -> TheaterParser -> IO (Maybe [Film])
fetch today (TP p name hn) =
  runReq defaultHttpConfig $ do
    bs <- req GET (https (pack hn)) NoReqBody bsResponse mempty
    pure (parse (p today) (decodeLatin1 (responseBody bs)))
  
run :: Date -> TheaterParser -> IO ()
run today tp@(TP _ name _) = do
  r <- fetch today tp
  case r of
    Just films -> do
      putStrLn name
      mapM_ printer films
    Nothing -> putStrLn ("failed to retrieve " ++ name ++ " showtimes")
  where
    printer (Film title times) = do
      putStrLn ("  " ++ unpack title)
      mapM_ stimes times
    stimes t = putStrLn ("    " ++ unpack t)
  
main :: IO ()
main = do
  today <- date
  mapM_ (run today) theaters
