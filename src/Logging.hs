{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Logging
Description : Contains logging functionality
Copyright   : (c) Adrian Hitz, 2019
Stability   : experimental
-}
module Logging where

import Data.Monoid ((<>))
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- |Prepends a time stamp to the given string and prints it to the console 
logToConsole :: T.Text -> IO ()
logToConsole s = do
    f <- formattedTime
    TIO.putStrLn (f <> " " <> s)

-- |Gets the current time and formats it according to 'timeFormat'
formattedTime :: IO T.Text
formattedTime = do
    currentTime <- getZonedTime
    return (T.pack (customFormatTime currentTime))

-- |Formats a 'ZonedTime' according to the format specified in 'timeFormat'
customFormatTime :: FormatTime t => t -> String
customFormatTime = formatTime defaultTimeLocale timeFormat

-- |Time format used by the logging functions
timeFormat = "%F %T" :: String
