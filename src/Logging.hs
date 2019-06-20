{-|
Module      : Logging
Description : Contains logging functionality
Copyright   : (c) Adrian Hitz, 2019
Stability   : experimental
-}
module Logging where

import Data.Time

-- |Prepends a time stamp to the given string and prints it to the console 
logToConsole :: String -> IO ()
logToConsole s = do
    currentTime <- getZonedTime
    let formattedTime = formatTime defaultTimeLocale "%F %T" currentTime
    putStrLn (formattedTime ++ " " ++ s)
