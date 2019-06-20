module DiscordExtensions where

import Discord

-- |Creates a mention of the given message's author (format: "<@userid>" where "userid" is the user's integer ID)
mentionAuthor :: Message -> String
mentionAuthor message = "<@" ++ (authorId message) ++ ">"

-- |Returns the handle of the given user
userHandle :: User -> String
userHandle user = (userName user) ++ "#" ++ (userDiscrim user)

-- |Returns the handle of the given message's author
authorHandle :: Message -> String
authorHandle message = userHandle (messageAuthor message)

-- |Returns the user ID of the given message's author
authorId :: Message -> String
authorId message = show (userId (messageAuthor message))

-- |Returns true if the given message was sent by a bot
fromBot :: Message -> Bool
fromBot message = userIsBot (messageAuthor message)
