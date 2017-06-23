{-# LANGUAGE OverloadedStrings #-}

module Client
  ( getFixerUrl
  ) where

-- Our handy module.
import Network.Wreq

import Control.Exception (IOException)

-- Operators such as (&) and (.~).
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Writer
import Data.Aeson
import Data.ByteString.Lazy
import Network.HTTP.Client (HttpException)
import Types

import Logger

getBasic :: String -> EitherT HttpException (WriterT String IO) ByteString
getBasic url = do
  msg <- liftIO . infoMsgIO $ " Sent GET request to " ++ (show url)
  lift . tell $ msg
  (catch
     ((liftIO $ get url) >>= (\x -> right $ x ^. responseBody))
     (\exception -> do
        msg <- liftIO $ errorMsgIO . displayException $ exception
        lift . tell $ msg
        left exception))

-- decoding error possibility
getFixerUrl :: String -> EitherT String (WriterT String IO) FixerResponse
getFixerUrl url =
  getBasic url & bimapEitherT displayException id >>=
  (\x ->
     case (eitherDecode x) of
       Left y ->
         (do msg <- liftIO $ errorMsgIO y
             lift . tell $ msg
             left y)
       Right z ->
         (do msg <- liftIO $ infoMsgIO $ show z
             lift . tell $ msg
             right z))
