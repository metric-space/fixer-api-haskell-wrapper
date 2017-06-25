module Utils
  ( parseFixerDate
  ) where

import Text.Parsec
import Text.Parsec.Char (digit, char, oneOf)
import Text.Parsec.Combinator (count)
import Types (FixerDate(..))

parseFixerDate :: String -> Either ParseError FixerDate
parseFixerDate x = runParser pp "" "" x
  where
    pp =
      (do year1 <- (oneOf "12")
          year2 <- (count 3 digit)
          _ <- char '-'
          month1 <- (oneOf "01")
          month2 <- digit
          _ <- char '-'
          day1 <- (oneOf "0123")
          day2 <- digit
          let year = (read $ year1 : year2) :: Int
              month = (read $ month1 : month2 : []) :: Int
              day = (read $ day1 : day2 : []) :: Int
          return $ FixerDate year month day)
