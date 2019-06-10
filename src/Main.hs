{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Monoid ((<>))
import System.IO
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


import Discord

utherQuote :: T.Text
utherQuote = "You are not my king yet, boy! Nor would I obey that command even if you were!"

main :: IO ()
main = do
    -- Causes text to printed to the console immediately instead of when the program terminates
    hSetBuffering stdout LineBuffering

    putStrLn "Starting bot!"
    token <- T.strip <$> TIO.readFile "./auth_token"
    dis <- loginRestGateway (Auth token)
    finally (handleEvent dis)
            (stopDiscord dis)

handleEvent :: (RestChan, Gateway, z) -> IO ()
handleEvent dis = do
    event <- nextEvent dis
    case event of
        Left er -> putStrLn ("Event error: " <> show er)
        Right (MessageCreate message) -> do
            when ((not (fromBot message)) && (msgIsCommand message)) $ do
                -- Log command sent by user to console
                logToConsole ((authorHandle message) ++ ": " ++ (show (messageText message)))
                -- Send response
                resp <- (restCall dis (CreateMessage (messageChannel message) utherQuote))
                -- Log response to console
                case resp of
                    Left error -> putStrLn ("REST error: " <> show error)
                    Right message -> logToConsole ("fprod-bot: " ++ (show (messageText message)))
            handleEvent dis
        _ -> handleEvent dis

-- | Returns true if the given message was sent by a bot
fromBot :: Message -> Bool
fromBot message = userIsBot (messageAuthor message)

-- | Returns the handle of the user who sent the given message
authorHandle :: Message -> String
authorHandle message = userHandle (messageAuthor message)

-- | Returns the handle of the given user
userHandle :: User -> String
userHandle user = (userName user) ++ "#" ++ (userDiscrim user)

-- | Returns true if the given string is a bot command (starts with '!' followed by at least one other non-space character)
isCommand :: String -> Bool
isCommand [] = False
isCommand [c] = False
isCommand (c:cs) = (c == '!') && (head cs) /= ' '

-- | Returns true if the given message is a bot command
msgIsCommand :: Message -> Bool
msgIsCommand message = isCommand (T.unpack (messageText message))

-- | Prepends a time stamp to the given string and prints it to the console 
logToConsole :: String -> IO ()
logToConsole s = do
    currentTime <- getZonedTime
    let formattedTime = formatTime defaultTimeLocale "%F %T" currentTime
    putStrLn (formattedTime ++ " " ++ s)
