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
    currentTime <- getZonedTime
    let formattedTime = T.pack (formatTime defaultTimeLocale "%F %T" currentTime)
    TIO.putStrLn (formattedTime <> " " <> s)
