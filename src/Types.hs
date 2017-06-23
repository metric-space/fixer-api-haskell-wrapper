{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Country(..)
  , FixerResponse(..)
  ) where

import Data.Aeson
import Data.Map.Lazy (Map)
import qualified Data.Text as Text
import GHC.Generics

data Country
  = AUD
  | CAD
  | CHF
  | CYP
  | CZK
  | DKK
  | EEK
  | GBP
  | HKD
  | HUF
  | ISK
  | JPY
  | KRW
  | LTL
  | LVL
  | MTL
  | NOK
  | NZD
  | PLN
  | ROL
  | SEK
  | SGD
  | SIT
  | SKK
  | TRL
  | USD
  | ZAR
  deriving (Show, Read, Generic, Ord, Eq)

instance FromJSON Country

instance ToJSON Country

instance FromJSONKey Country where
  fromJSONKey = FromJSONKeyText (read . Text.unpack)

instance ToJSONKey Country

data FixerResponse = FixerResponse
  { base :: Country
  , date :: String
  , rates :: Map Country Float
  } deriving (Generic, Eq, Show)

instance FromJSON FixerResponse

instance ToJSON FixerResponse
