# discord-bot
A small Discord bot written in Haskell for a functional programming class.

## Features
The bot can recognise commands and respond to them. It currently supports the following commands:

- `!commands` to show a list of available commands
- `!flip` flips a coin
- `!roll` rolls a 20-sided die
- `!roll n` rolls an n-sided die, where n is an integer from 2 to 1,000,000.


## Planned Features
A command that lets users set their own role inside a channel was originally planned, but could unfortunately not be
implemented because the library [discord-haskell](https://github.com/aquarial/discord-haskell) didn't provide the
required functionality yet.

## Dependencies

- [discord-haskell](https://github.com/aquarial/discord-haskell)

## Usage

To run the bot, create a file called `auth_token` in the project's root directory and copy your Discord bot's
authentication token into this file. You can get such a token by creating a bot on
[Discord's developer portal](https://discordapp.com/developers/applications/) if you don't already have one. Make sure
you don't accidentally share it!

Build and run the project using [Stack](https://www.haskellstack.org):

```
$ stack run
```
