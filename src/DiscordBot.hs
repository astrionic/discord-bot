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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

import BotCmd
import Coin
import DiscordExtensions
import Logging

-- |Text listing the supported commands
cmdListText :: T.Text
cmdListText = "I support the following commands:\n\
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
        Right (MessageCreate msg) -> do
            handleMessage msg discord
            handleEvents discord
        _ -> handleEvents discord

-- |Handles messages that are sent to the bot (including messages that are sent on a server where the bot is present)
handleMessage :: Message -> (RestChan, Gateway, z) -> IO ()
handleMessage msg discord = do
    when ((not (isFromBot msg)) && (msgIsCommand msg)) $ do
        -- Log command sent by user to console
        logToConsole ((authorHandle msg) <> ": " <> (messageText msg))
        let maybeBotCmd = parseCmd (messageText msg)
        case maybeBotCmd of
            Just botCmd -> handleCommand botCmd msg discord
            Nothing -> return ()

-- |Handles commands that are the bot receives
handleCommand :: BotCmd -> Message -> (RestChan, Gateway, z) -> IO ()
handleCommand botCmd msg discord = case botCmd of
    BotCmd CmdList _    -> respond cmdListText msg discord
    BotCmd CmdFlip _    -> sendFlipResponse msg discord
    BotCmd CmdRole _    -> respond "Not implemented yet" msg discord
    BotCmd CmdRoll args -> handleCmdRoll args msg discord
    _                   -> return ()

-- |Returns true if the given message is a bot command
msgIsCommand :: Message -> Bool
msgIsCommand msg = isCommand $ messageText msg

-- |Responds to a message with a given text (in the same channel) and logs the response to the console
respond :: T.Text -> Message -> (RestChan, Gateway, z) -> IO ()
respond responseText msg discord = do
    response <- (restCall discord (CreateMessage (messageChannel msg) responseText))
    case response of
        Left error -> logToConsole ("ERROR: " <> tshow error)
        Right msg -> logToConsole ("fprod-bot: " <> (messageText msg))

-- |Prepends a mention of the given message's author to the given text and then responds with it to the message
respondWithMention :: T.Text -> Message -> (RestChan, Gateway, z) -> IO ()
respondWithMention responseText msg discord = do
    let responseTextWithMention = ((mentionAuthor msg) <> " " <> responseText)
    respond responseTextWithMention msg discord

-- |Flips a coin and sends the result to the sender of the given message (in the same channel)
sendFlipResponse :: Message -> (RestChan, Gateway, z) -> IO ()
sendFlipResponse msg discord = do
    n <- flipCoin
    respondWithMention (tshow n) msg discord

-- |Handles 'CmdRoll'. Calls 'sendRoll' if valid arguments (or no arguments) were passed.
handleCmdRoll :: [T.Text] -> Message -> (RestChan, Gateway, z) -> IO ()
handleCmdRoll [] msg discord = sendRollResponse 20 msg discord
handleCmdRoll (arg:args) msg discord = do
    case TR.decimal arg of
        Right (n, _) -> sendRollResponse n msg discord
        Left _ -> respond invRollArgResponse msg discord

-- |Generates a random number and sends it in a response to the given 'Message'.
sendRollResponse :: Int -> Message -> (RestChan, Gateway, z) -> IO ()
sendRollResponse n msg discord
    | n >= rollLowerBound && n <= rollUpperBound = do
        rndInt <- randomRIO (rollLowerBound, n)
        let responseText = " rolls " <> tshow rndInt <> " (1-" <> tshow n <> ")"
        respondWithMention responseText msg discord
    | otherwise = respond invRollArgResponse msg discord

-- |Converts a value of a type that is an instance of 'Show' to 'Data.Text.Text'
tshow :: Show a => a -> T.Text
tshow a = T.pack $ show a
