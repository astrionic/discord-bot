{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Monoid ((<>))
import Data.Time
import Discord
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

utherQuote :: T.Text
utherQuote = "You are not my king yet, boy! Nor would I obey that command even if you were!"

commandsText :: T.Text
commandsText = "I support the following commands:\n\
               \`!commands` display this message\n\
               \`!rank` does absolutely nothing"

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
                case T.tail (messageText message) of
                    "commands" -> respond commandsText message dis
                    _ -> respond utherQuote message dis
            handleEvent dis
        _ -> handleEvent dis

-- | Responds to a message with a given text and logs the response to the console
respond :: T.Text -> Message -> (RestChan, Gateway, z) -> IO ()
respond responseText message dis = do
    response <- (restCall dis (CreateMessage (messageChannel message) responseText))
    case response of
        Left error -> putStrLn ("ERROR: " <> show error)
        Right message -> logToConsole ("fprod-bot: " ++ (show (messageText message)))

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
