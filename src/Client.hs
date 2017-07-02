{-# LANGUAGE OverloadedStrings #-}

module Client
  ( getFixerUrl
  ) where

import Control.Exception hiding (catch)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Data.Aeson
import Data.Bifunctor
import Data.ByteString.Lazy
import Network.HTTP.Client (HttpException)
import Network.Wreq

import Logger
import Types

getBasic :: String -> ExceptT SomeException (WriterT String IO) ByteString
getBasic url = do
  msg <- liftIO . infoMsgIO $ " Sent GET request to " ++ (show url)
  lift . tell $ msg
  (catch
     (do resp <- liftIO $ fmap (\x -> x ^. responseBody) . get $ url
         msg <- liftIO $ infoMsgIO $ " Server sent back " ++ (show resp)
         lift . tell $ msg
         return resp)
     (\exception -> do
        msg <- liftIO $ errorMsgIO . displayException $ (exception :: HttpException)
        lift . tell $ msg
        throwE $ toException exception))

-- decoding error possibility
parseBodyToFixerResponse :: ByteString -> ExceptT SomeException (WriterT String IO) FixerResponse
parseBodyToFixerResponse bbody = do
  let body = eitherDecode bbody
  case body of
    Right x -> do
      msg <- liftIO $ infoMsgIO $ show x
      lift . tell $ msg
      return x
    Left c -> do
      msg <- liftIO $ errorMsgIO $ c
      lift . tell $ msg
      throwE . toException $ MyParseException msg

getFixerUrl :: String -> ExceptT SomeException (WriterT String IO) FixerResponse
getFixerUrl url = getBasic url >>= parseBodyToFixerResponse
