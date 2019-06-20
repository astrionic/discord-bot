{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : DiscordBot
Description : A small Discord bot with the ability to respond to commands, flip coins and roll dice.
Copyright   : (c) Adrian Hitz, 2019
Stability   : experimental
-}
module DiscordBot where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Monoid ((<>))
import Discord
import System.Directory
import System.IO
import System.Random
import Text.Read
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Coin
import DiscordExtensions
import Logging

-- |Text sent by the bot when an unknown command is used
disobeyText :: T.Text
disobeyText = "You are not my king yet, boy! Nor would I obey that command even if you were!"

-- |Text listing the supported commands
commandsText :: T.Text
commandsText = "I support the following commands:\n\
               \`!commands` displays this message.\n\
               \`!role` is not implemented yet.\n\
               \`!flip` flips a coin.\n\
               \`!roll 20` rolls a 20-sided die. Also works with any other integer from `1` to `1000000`."

-- |Starts the bot
startBot :: IO ()
startBot = do
    -- Causes text to be printed to the console immediately instead of when the program terminates
    hSetBuffering stdout LineBuffering

    logToConsole "Starting bot!"
    maybeToken <- readAuthToken
    case maybeToken of
        Nothing -> logToConsole "ERROR: Cant find auth_token."
        Just token -> do
            discord <- loginRestGateway (Auth token)
            finally (handleEvents discord)
                    (stopDiscord discord)

-- |Reads the auth token from a file. Returns 'Nothing' if the file doesn't exist.
readAuthToken :: IO (Maybe T.Text)
readAuthToken = do
    fileExists <- doesFileExist "./auth_token"
    if fileExists
    then do
        token <- T.strip <$> TIO.readFile "./auth_token"
        return (Just token)
    else
        return Nothing

-- |Event loop
handleEvents :: (RestChan, Gateway, z) -> IO ()
handleEvents discord = do
    event <- nextEvent discord
    case event of
        Left error -> logToConsole ("ERROR: " <> T.pack (show error))
        Right (MessageCreate message) -> do
            when ((not (fromBot message)) && (msgIsCommand message)) $ do
                -- Log command sent by user to console
                logToConsole ((authorHandle message) <> ": " <> (messageText message))
                case T.tail (messageText message) of
                    "commands" -> respond commandsText message discord
                    "role" -> respond "Not implemented yet" message discord
                    "flip" -> sendFlip message discord
                    "roll" -> respond ((mentionAuthor message) <>" The `!roll` command \
                        \requires an argument (e.g. `!roll 20`).") message discord
                    otherCmd -> handleOtherCmd message discord
            handleEvents discord
        _ -> handleEvents discord

-- |Tries to read an int from a 'Data.Text.Text'
readMaybeInt :: T.Text -> Maybe Int
readMaybeInt t = readMaybe (T.unpack t)

-- |Flips a coin and sends the result to the sender of the given message (in the same channel)
sendFlip :: Message -> (RestChan, Gateway, z) -> IO ()
sendFlip message discord = do
    n <- flipCoin
    respond (mentionAuthor message <> " " <> T.pack (show n)) message discord

-- |Responds to a message with a given text (in the same channel) and logs the response to the console
respond :: T.Text -> Message -> (RestChan, Gateway, z) -> IO ()
respond responseText message discord = do
    response <- (restCall discord (CreateMessage (messageChannel message) responseText))
    case response of
        Left error -> logToConsole ("ERROR: " <> T.pack (show error))
        Right message -> logToConsole ("fprod-bot: " <> (messageText message))


-- |Returns true if the given string is a bot command (starts with '!' followed by at least one other non-space character)
isCommand :: String -> Bool
isCommand [] = False
isCommand [c] = False
isCommand (c:cs) = (c == '!') && (head cs) /= ' '

-- |Returns true if the given message is a bot command
msgIsCommand :: Message -> Bool
msgIsCommand message = isCommand (T.unpack (messageText message))

-- TODO Clean this mess up.
-- |Handles longer commands.
handleOtherCmd :: Message -> (RestChan, Gateway, z) -> IO ()
handleOtherCmd msg dis = if (T.isPrefixOf "roll " (T.tail (messageText msg))) && (length (T.words (T.tail (messageText msg)))) >= 2
                         then do
                            let arg = (T.words (T.tail (messageText msg))) !! 1
                            let rndUpperLimit = readMaybeInt arg
                            case rndUpperLimit of
                                Just n -> if n >= 1 && n <= 1000000
                                            then do
                                            rndInt <- randomRIO (1, n)
                                            respond (mentionAuthor msg <> " rolls " <> T.pack (show rndInt) <> " (1-" <> T.pack (show n) <> ")") msg dis
                                            else do
                                            respond ((mentionAuthor msg) <> " Invalid argument, integer between 1 and 1000000 required.") msg dis
                                Nothing -> do
                                    respond ((mentionAuthor msg) <> " Invalid argument, integer between 1 and 1000000 required.") msg dis
                         else respond disobeyText msg dis
