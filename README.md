# Echo bot for Telegram & Vk.com
This repo contains my attempt to learn haskell by creating simple echo bot with simple API.
```shell
stack build
stack test
stack exec bot-exe -- --config .conf
```

## Configuration
The bot can be configured via these three sources in order of decreasing priority:
### Command line arguments
Option name is prefixed by `--`
```shell
bot --config .conf --tg.token 123456:AaseDG55y
```
### Environment variables
In the option name `.` is replaced by `_`, and the whole name is prefixed with `BOT_`
```shell
BOT_tg_token=123456:AaseDG55y bot
```
### Config file
Use `--config` parameter to specify config file. It consists of key value pairs, separated by `=`. Supports comments, starting with `#` and multiline strings. Quotes `"` are optional for values.
```
# Config file for the bot

# tg.proxy  = 127.0.0.1:9080
tg.token  = 1258788986:AAErwZlr

logLevel = Debug

helpText = "
Hi,
what did you wanted?
"
```

## Options
Per API options can be used with `tg.` or `vk.` prefix. If used as is the option applied to both API's
| Option        | Type       | Value                                     | Default   | Description                   |
|---------------|------------|-------------------------------------------|-----------|-------------------------------|
| `helpText`    | Global     | String                                    | Some text | `/help` command answer        |
| `logLevel`    | Per API    | `Debug` \| `Info` \| `Warning` \| `Error` | `Warning` | Log level                     |
| `proxy`       | Per API    | `localhost:5000`                          | --        | Http proxy host and port      |
| `repeatTimes` | Global     | Integer                                   | 5         | Initial number of repetitions |
| `tg.token`    | For Tg API | String                                    | --        | Telegram bot token            |
| `vk.groupId`  | For Vk API | Integer                                   | --        | Vkontakte group id            |
| `vk.token`    | For Vk API | String                                    | --        | Vkontakte group token         |
