{-# LANGUAGE OverloadedStrings #-}

module Server
  ( appRoutes
  ) where

import Client
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Data.Either
import Data.Monoid
import Logger
import Network.HTTP.Types.Status
import Text.Read (readMaybe)
import Types
import Utils
import Web.Scotty

baseUrl :: String
baseUrl = "https://api.fixer.io/"

latestUrl :: String
latestUrl = baseUrl ++ "latest"

historicalUrl :: String -> String
historicalUrl = (++) baseUrl

setBaseCountryUrl :: Country -> String
setBaseCountryUrl x = mconcat [baseUrl, "latest?base=", show x]

convertBetweenUrl :: Country -> Country -> String
convertBetweenUrl x y = mconcat [baseUrl, "latest?symbols=", show x, ",", show y]

commonAction :: String -> ActionM ()
commonAction url =
  (do x <- liftIO (runStdoutLoggingT . runExceptT . getFixerUrl $ url)
      either (\x -> status status500 >> text "Please try later") json x)

-- routes
getLatest :: ScottyM ()
getLatest = get "/" (commonAction latestUrl)

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
          Right x -> commonAction $ historicalUrl date)

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
          Just code -> commonAction . setBaseCountryUrl $ code)

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
          Just [c1, c2] -> commonAction $ convertBetweenUrl c1 c2)

-- routes are sequentially tried till the correct route gets matched
appRoutes :: ScottyM ()
appRoutes = do
  getLatest
  getWithDate
  getWithBase
  getConvertFromBaseTo
