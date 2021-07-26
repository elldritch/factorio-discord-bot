# factorio-discord-bot

This bot reads the stdout of a headless Factorio server, and sends messages to a Discord channel when someone joins or leaves the server.

## Usage

```sh
$ factorio-discord-bot \
  --factorio-stdout-file=$STDOUT_FILE \
  --discord-webhook-url=$WEBHOOK_URL
```
