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

import BotCmd
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
               \`!roll 20` rolls a 20-sided die. Also works with any other integer from `"
               <> tshow rollLowerBound <> "` to `"<> tshow rollUpperBound <> "` (inclusive)."

-- |Path to the authentication token
authTokenPath = "./auth_token" :: String

-- |Lower bound for random number generation for dice rolls
rollLowerBound = 2 :: Int;

-- |Upper bound for random number generation for dice rolls
rollUpperBound = 10^6 :: Int;

-- |Message that is sent if a user uses an invalid argument for 'CmdRoll'
invRollArgResponse :: T.Text
invRollArgResponse = "Invalid argument, integer between " <> tshow rollLowerBound <> " and "
    <> tshow rollUpperBound <> " required."

-- |Starts the bot
startBot :: IO ()
startBot = do
    -- Causes text to be printed to the console immediately instead of when the program terminates
    hSetBuffering stdout LineBuffering

    logToConsole "Starting bot!"
    maybeToken <- readAuthToken authTokenPath
    case maybeToken of
        Nothing -> logToConsole "ERROR: Cant find auth_token."
        Just token -> do
            discord <- loginRestGateway (Auth token)
            finally (handleEvents discord)
                    (stopDiscord discord)

-- |Reads the auth token from a file. Returns 'Nothing' if the file doesn't exist.
readAuthToken :: String -> IO (Maybe T.Text)
readAuthToken path = do
    fileExists <- doesFileExist path
    if fileExists
    then do
        token <- T.strip <$> TIO.readFile path
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
            handleMessage message discord
            handleEvents discord
        _ -> handleEvents discord

-- |Handles messages that are sent to the bot (including messages that are sent on a server where the bot is present)
handleMessage :: Message -> (RestChan, Gateway, z) -> IO ()
handleMessage message discord = do
    when ((not (fromBot message)) && (msgIsCommand message)) $ do
        -- Log command sent by user to console
        logToConsole ((authorHandle message) <> ": " <> (messageText message))
        let maybeBotCmd = parseCmd (messageText message)
        case maybeBotCmd of
            Just botCmd -> handleCommand botCmd message discord
            Nothing -> return ()

-- |Handles commands that are the bot receives
handleCommand :: BotCmd -> Message -> (RestChan, Gateway, z) -> IO ()
handleCommand botCmd message discord = case botCmd of
    BotCmd CmdList _ -> respond commandsText message discord
    BotCmd CmdFlip _ -> sendFlip message discord
    BotCmd CmdRole _ -> respond "Not implemented yet" message discord
    BotCmd CmdRoll args -> handleCmdRoll args message discord
    _ -> return ()

-- |Returns true if the given message is a bot command
msgIsCommand :: Message -> Bool
msgIsCommand message = isCommand $ messageText message

-- |Responds to a message with a given text (in the same channel) and logs the response to the console
respond :: T.Text -> Message -> (RestChan, Gateway, z) -> IO ()
respond responseText message discord = do
    response <- (restCall discord (CreateMessage (messageChannel message) responseText))
    case response of
        Left error -> logToConsole ("ERROR: " <> T.pack (show error))
        Right message -> logToConsole ("fprod-bot: " <> (messageText message))

-- |Prepends a mention of the given message's author to the given text and then responds with it to the message
respondWithMention :: T.Text -> Message -> (RestChan, Gateway, z) -> IO ()
respondWithMention responseText msg discord = do
    let responseTextWithMention = ((mentionAuthor msg) <> " " <> responseText)
    respond responseTextWithMention msg discord

-- |Flips a coin and sends the result to the sender of the given message (in the same channel)
sendFlip :: Message -> (RestChan, Gateway, z) -> IO ()
sendFlip message discord = do
    n <- flipCoin
    respond (mentionAuthor message <> " " <> T.pack (show n)) message discord

-- |Handles 'CmdRoll'. Calls 'sendRoll' if valid arguments (or no arguments) were passed.
handleCmdRoll :: [T.Text] -> Message -> (RestChan, Gateway, z) -> IO ()
handleCmdRoll [] msg discord = sendRoll 20 msg discord
handleCmdRoll (arg:args) msg discord = do
    case readMaybeInt arg of
        Just n -> sendRoll n msg discord
        Nothing -> respond invRollArgResponse msg discord

-- |Generates a random number and sends it in a response to the given 'Message'.
sendRoll :: Int -> Message -> (RestChan, Gateway, z) -> IO ()
sendRoll n msg discord
    | n >= rollLowerBound && n <= rollUpperBound = do
        rndInt <- randomRIO (rollLowerBound, n)
        let responseText = " rolls " <> tshow rndInt <> " ( " <> tshow rollLowerBound <> "-" <> tshow n <> ")"
        respondWithMention responseText msg discord
    | otherwise = respond invRollArgResponse msg discord

-- |Tries to read an int from a 'Data.Text.Text'
readMaybeInt :: T.Text -> Maybe Int
readMaybeInt = readMaybe . T.unpack

-- |Converts a value of a type that is an instance of 'Show' to 'Data.Text.Text'
tshow :: Show a => a -> T.Text
tshow a = T.pack $ show a