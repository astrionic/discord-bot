{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : DiscordExtensions
Description : Contains some useful functions related to the discord-haskell library.
Copyright   : (c) Adrian Hitz, 2019
Stability   : experimental
-}
module DiscordExtensions where

import Data.Monoid ((<>))
import Discord
import qualified Data.Text as T

-- |Creates a mention of the given message's author (format: "<@userid>" where "userid" is the user's integer ID)
mentionAuthor :: Message -> T.Text
mentionAuthor msg = "<@" <> authorId msg <> ">"

-- |Returns the handle of the given user
userHandle :: User -> T.Text
userHandle user = T.pack ((userName user) ++ "#" ++ (userDiscrim user))

-- |Returns the handle of the given message's author
authorHandle :: Message -> T.Text
authorHandle msg = userHandle (messageAuthor msg)

-- |Returns the user ID of the given message's author
authorId :: Message -> T.Text
authorId msg = T.pack (show (userId (messageAuthor msg)))

-- |Returns true if the given message was sent by a bot
isFromBot :: Message -> Bool
isFromBot msg = userIsBot (messageAuthor msg)
