# discord-bot
A general-purpose Discord bot written in Haskell for a functional programming class.


## Usage

To run the bot, create a file called `auth_token` in the project's root directory and copy your Discord bot's authentication token into this file. You can get such a token by creating a bot on [Discord's developer portal](https://discordapp.com/developers/applications/) if you don't already have one. Make sure you don't accidentally share it!

Run the program using cabal:

```
$ cabal v2-run
```