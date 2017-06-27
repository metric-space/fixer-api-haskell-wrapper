{-# LANGUAGE OverloadedStrings #-}

module Server
  ( appRoutes
  ) where

import Client
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Writer
import Data.Either
import Data.Monoid
import Logger
import Network.HTTP.Types.Status
import Text.Read (readMaybe)
import Types
import Utils
import Web.Scotty

constant_base_url :: String
constant_base_url = "https://api.fixer.io/"

constant_latest_url :: String
constant_latest_url = constant_base_url ++ "latest"

constant_historical_url :: String -> String
constant_historical_url = (++) constant_base_url

constant_set_base_url :: Country -> String
constant_set_base_url x = mconcat [constant_base_url, "latest?base=", show x]

constant_convert_between_url :: Country -> Country -> String
constant_convert_between_url x y =
  mconcat [constant_base_url, "latest?symbols=", show x, ",", show y]

commonAction :: String -> ActionM ()
commonAction url =
  (do (x, logs) <- liftIO (runWriterT . runEitherT . getFixerUrl $ url)
      liftIO (putStr logs)
      either (\x -> status status500 >> text "Please try later") json x)

-- routes
getLatest :: ScottyM ()
getLatest = get "/" (commonAction constant_latest_url)

getWithDate :: ScottyM ()
getWithDate =
  get
    "/date/:date"
    (do date <- param "date"
        case (parseFixerDate date) of
          Left y ->
            (do liftIO
                  $(errorMsgIO $
                    "Date :" ++
                    date ++ " sent was/is not valid \n error msg: " ++ "{{" ++ (show y) ++ "}}") >>=
                  putStr
                status status500
                text "Invalid Date")
          Right x -> commonAction $ constant_historical_url date)

getWithBase :: ScottyM ()
getWithBase =
  get
    "/:base"
    (do base <- param $ "base"
        case (readMaybe base :: Maybe Country) of
          Nothing ->
            (do liftIO $(errorMsgIO $ "Country Code :" ++ base ++ " sent was/is not valid") >>=
                  putStr
                status status500
                text "Check Country Code")
          Just code -> commonAction . constant_set_base_url $ code)

getConvertFromBaseTo :: ScottyM ()
getConvertFromBaseTo =
  get
    "/:base/:to"
    (do codes@[base, to] <- mapM param ["base", "to"]
        case (mapM readMaybe codes :: Maybe [Country]) of
          Nothing ->
            (do liftIO $
                  (errorMsgIO $
                   "One or both Country codes (" ++ base ++ "," ++ to ++ ") sent were/are not valid") >>=
                  putStr
                status status500
                text "Check Country Code")
          Just [c1, c2] -> commonAction $ constant_convert_between_url c1 c2)

-- routes are sequentially tried till the correct route gets matched
appRoutes :: ScottyM ()
appRoutes = do
  getLatest
  getWithDate
  getWithBase
  getConvertFromBaseTo
