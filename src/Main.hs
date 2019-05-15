{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Monoid ((<>))
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

main :: IO ()
main = do
    -- Causes text to printed to the console immediately instead of when the program terminates
    hSetBuffering stdout LineBuffering

    putStrLn "Starting bot!"
    token <- T.strip <$> TIO.readFile "./auth_token"
    dis <- loginRestGateway (Auth token)
    finally (echo dis)
            (stopDiscord dis)

echo :: (RestChan, Gateway, z) -> IO ()
echo dis = do
    e <- nextEvent dis
    case e of
        Left er -> putStrLn ("Event error: " <> show er)
        Right (MessageCreate message) -> do
            when (not (fromBot message)) $ do
                resp <- restCall dis (CreateMessage (messageChannel message) (messageText message))
                putStrLn ((authorHandle message) ++ ": " ++ (show (messageText message)))
            echo dis
        _ -> echo dis

fromBot :: Message -> Bool
fromBot message = userIsBot (messageAuthor message)

authorHandle :: Message -> String
authorHandle message = userHandle (messageAuthor message)

userHandle :: User -> String
userHandle user = (userName user) ++ "#" ++ (userDiscrim user)
