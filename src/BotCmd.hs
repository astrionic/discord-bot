{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : BotCmd
Description : Contains data structures and functions related to bot commands
Copyright   : (c) Adrian Hitz, 2019
Stability   : experimental
-}
module BotCmd where

import qualified Data.Text as T

-- |Represents the different types of commands which the bot understands
data CmdType = CmdFlip | CmdList | CmdRole | CmdRoll | CmdUnknown

-- |Represents a complete bot command with it's 'CmdType' and list of arguments
data BotCmd = BotCmd CmdType [T.Text]

-- |Turns a given 'Data.Text.Text' into the corresponding 'CmdType' 
parseCmdType :: T.Text -> CmdType
parseCmdType "flip"     = CmdFlip
parseCmdType "commands" = CmdList
parseCmdType "role"     = CmdRole
parseCmdType "roll"     = CmdRoll
parseCmdType _          = CmdUnknown

-- |Parses a given 'Data.Text.Text' and returns the corresponding 'BotCmd' if it contained a correctly formatted
-- command.
parseCmd :: T.Text -> Maybe BotCmd
parseCmd t =
    if isCommand t
    then
        let w = T.words (T.tail t)
        in Just (BotCmd (parseCmdType (head w)) (tail w)) 
    else
        Nothing

-- |Returns true if the given 'Data.Text.Text' contains a valid bot command. A valid command starts with "!" followed by
-- at least one more character.
isCommand :: T.Text -> Bool
isCommand t = (T.head t) == '!' && T.length t > 1