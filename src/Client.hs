{-# LANGUAGE OverloadedStrings #-}

module Client
  ( getFixerUrl
  ) where

import Control.Exception hiding (catch)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Data.Aeson
import qualified Data.Text as T
import Data.Bifunctor
import Data.ByteString.Lazy
import Network.HTTP.Client (HttpException)
import Network.Wreq

import Types


getBasic :: String -> ExceptT SomeException (LoggingT IO) ByteString
getBasic url = do
  logInfoN . T.pack $ " Sent GET request to " ++ (show url)
  (catch
     (do resp <- liftIO $ fmap (\x -> x ^. responseBody) . get $ url
         logInfoN . T.pack  $ " Server sent back " ++ (show resp)
         return resp)
     (\exception -> do
        logErrorN . T.pack $ displayException $ (exception :: HttpException)
        throwE $ toException exception))

-- decoding error possibility
parseBodyToFixerResponse :: ByteString -> ExceptT SomeException (LoggingT IO) FixerResponse
parseBodyToFixerResponse bbody = do
  let body = eitherDecode bbody
  case body of
    Right x -> do
      logInfoN . T.pack $ show x
      return x
    Left c -> do
      logErrorN . T.pack $ c
      throwE . toException $ MyParseException c

getFixerUrl :: String -> ExceptT SomeException (LoggingT IO) FixerResponse
getFixerUrl url = getBasic url >>= parseBodyToFixerResponse
