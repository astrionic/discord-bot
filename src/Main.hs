{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Monoid ((<>))
import Data.Time
import Discord
import System.IO
import System.Random
import Text.Read
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Coin

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

main :: IO ()
main = do
    -- Causes text to printed to the console immediately instead of when the program terminates
    hSetBuffering stdout LineBuffering

    logToConsole "Starting bot!"
    token <- T.strip <$> TIO.readFile "./auth_token"
    discord <- loginRestGateway (Auth token)
    finally (handleEvents discord)
            (stopDiscord discord)

-- |Event loops
handleEvents :: (RestChan, Gateway, z) -> IO ()
handleEvents discord = do
    event <- nextEvent discord
    case event of
        Left error -> logToConsole ("ERROR: " ++ show error)
        Right (MessageCreate message) -> do
            when ((not (fromBot message)) && (msgIsCommand message)) $ do
                -- Log command sent by user to console
                logToConsole ((authorHandle message) ++ ": " ++ (show (messageText message)))
                case T.tail (messageText message) of
                    "commands" -> respond commandsText message discord
                    "role" -> respond "Not implemented yet" message discord
                    "flip" -> sendFlip message discord
                    "roll" -> respond "The `!roll` command requires an argument (e.g. `!roll 20`)." message discord
                    otherCmd -> if (T.isPrefixOf "roll " (T.tail (messageText message))) && (length (words (tail (T.unpack (messageText message))))) >= 2
                         then do
                            let arg = (words (tail (T.unpack (messageText message)))) !! 1
                            let rndUpperLimit = readMaybeInt arg
                            case rndUpperLimit of
                                Just n -> if n >= 1 && n <= 1000000
                                          then do
                                            rndInt <- randomRIO (1, n)
                                            respond (T.pack (mentionAuthor message ++ " rolls " ++ show rndInt ++ " (1-" ++ (show n) ++ ")")) message discord
                                          else do
                                            respond (T.pack ((mentionAuthor message) ++ " Invalid argument, integer between 1 and 1000000 required.")) message discord
                                Nothing -> do
                                    respond (T.pack ((mentionAuthor message) ++ " Invalid argument, integer between 1 and 1000000 required.")) message discord
                         else respond disobeyText message discord
            handleEvents discord
        _ -> handleEvents discord

-- |Tries to read an int from a string
readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

-- |Flips a coin and sends the result to the sender of the given message (in the same channel)
sendFlip :: Message -> (RestChan, Gateway, z) -> IO ()
sendFlip message discord = do
    n <- flipCoin
    respond (T.pack (mentionAuthor message ++ " " ++ (show n))) message discord

-- |Creates a mention of the given message's author (format: "<@userid>" where "userid" is the user's integer ID)
mentionAuthor :: Message -> String
mentionAuthor message = "<@" ++ (authorId message) ++ ">"

-- |Responds to a message with a given text (in the same channel) and logs the response to the console
respond :: T.Text -> Message -> (RestChan, Gateway, z) -> IO ()
respond responseText message discord = do
    response <- (restCall discord (CreateMessage (messageChannel message) responseText))
    case response of
        Left error -> logToConsole ("ERROR: " ++ show error)
        Right message -> logToConsole ("fprod-bot: " ++ (show (messageText message)))

-- |Returns true if the given message was sent by a bot
fromBot :: Message -> Bool
fromBot message = userIsBot (messageAuthor message)

-- |Returns the user ID of the given message's author
authorId :: Message -> String
authorId message = show (userId (messageAuthor message))

-- |Returns the handle of the given message's author
authorHandle :: Message -> String
authorHandle message = userHandle (messageAuthor message)

-- |Returns the handle of the given user
userHandle :: User -> String
userHandle user = (userName user) ++ "#" ++ (userDiscrim user)

-- |Returns true if the given string is a bot command (starts with '!' followed by at least one other non-space character)
isCommand :: String -> Bool
isCommand [] = False
isCommand [c] = False
isCommand (c:cs) = (c == '!') && (head cs) /= ' '

-- |Returns true if the given message is a bot command
msgIsCommand :: Message -> Bool
msgIsCommand message = isCommand (T.unpack (messageText message))

-- |Prepends a time stamp to the given string and prints it to the console 
logToConsole :: String -> IO ()
logToConsole s = do
    currentTime <- getZonedTime
    let formattedTime = formatTime defaultTimeLocale "%F %T" currentTime
    putStrLn (formattedTime ++ " " ++ s)
