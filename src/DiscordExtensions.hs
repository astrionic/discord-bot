{-# LANGUAGE OverloadedStrings #-}

module DiscordExtensions where
{-|
Module      : DiscordExtensions
Description : Contains some useful functions related to the discord-haskell library.
Copyright   : (c) Adrian Hitz, 2019
Stability   : experimental
-}
import Data.Monoid ((<>))
import Discord
import qualified Data.Text as T

-- |Creates a mention of the given message's author (format: "<@userid>" where "userid" is the user's integer ID)
mentionAuthor :: Message -> T.Text
mentionAuthor message = "<@" <> authorId message <> ">"

-- |Returns the handle of the given user
userHandle :: User -> String
userHandle user = (userName user) ++ "#" ++ (userDiscrim user)

-- |Returns the handle of the given message's author
authorHandle :: Message -> T.Text
authorHandle message = T.pack (userHandle (messageAuthor message))

-- |Returns the user ID of the given message's author
authorId :: Message -> T.Text
authorId message = T.pack (show (userId (messageAuthor message)))

-- |Returns true if the given message was sent by a bot
fromBot :: Message -> Bool
fromBot message = userIsBot (messageAuthor message)
