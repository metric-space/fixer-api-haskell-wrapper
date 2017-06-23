module Logger
  ( errorMsg
  , errorMsgIO
  , infoMsg
  , infoMsgIO
  , warningMsg
  , warningMsgIO
  , debugMsg
  , debugMsgIO
  ) where

import Data.Time.LocalTime (getZonedTime)

msg :: String -> String -> String
msg type_ message = "[" ++ type_ ++ "]: " ++ message ++ "\n"

-- add timestamp here
msgIO :: String -> IO String
msgIO message = getZonedTime >>= \y -> return $ "[" ++ (show y) ++ "]" ++ message

errorMsg :: String -> String
errorMsg = msg "ERROR"

errorMsgIO :: String -> IO String
errorMsgIO = msgIO . errorMsg

infoMsg :: String -> String
infoMsg = msg "INFO"

infoMsgIO :: String -> IO String
infoMsgIO = msgIO . infoMsg

warningMsg :: String -> String
warningMsg = msg "WARNING"

warningMsgIO :: String -> IO String
warningMsgIO = msgIO . warningMsg

debugMsg :: String -> String
debugMsg = msg "DEBUG"

debugMsgIO :: String -> IO String
debugMsgIO = msgIO . debugMsg
