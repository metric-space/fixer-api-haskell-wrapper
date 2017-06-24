{-# LANGUAGE OverloadedStrings #-}

module Main where

import Client
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Writer
import Data.Either
import Data.Monoid
import Network.HTTP.Types.Status
import Types
import Web.Scotty

constant_base_url :: String
constant_base_url = "https://api.fixer.io/"

constant_latest_url :: String
constant_latest_url = constant_base_url ++ "latest"

constant_historical_url :: String -> String
constant_historical_url = (++) constant_base_url

constant_set_base_url :: Country -> String
constant_set_base_url = (++) constant_base_url . show

constant_convert_between_url :: Country -> Country -> String
constant_convert_between_url x y =
  mconcat [constant_base_url, "latest?symbols=", show x, ",", show y]

commonAction :: String -> ActionM ()
commonAction url =
  (do (x, logs) <- liftIO (runWriterT . runEitherT . getFixerUrl $ url)
      liftIO (putStrLn logs)
      either (\x -> status status500 >> text "Please try later") json x)

-- routes
getLatest :: ScottyM ()
getLatest = get "/" (commonAction constant_latest_url)

main :: IO ()
main = scotty 8000 getLatest
