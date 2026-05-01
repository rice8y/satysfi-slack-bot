# Docker Deployment Notes

This project runs as one public HTTP service. The bot renders in fresh
per-request directories by invoking the `satysfi` executable.

## Required Runtime Files

- `satysfi-slack-bot`: the bot executable.
- `satysfi`: available on `PATH` or configured with `SATYSFI_BOT_COMMAND`.
- `pdftoppm`: available on `PATH` or configured with `SATYSFI_BOT_PDFTOPPM_COMMAND`.
- `curl`: available on `PATH` or configured with `SATYSFI_BOT_CURL_COMMAND`.
- A writable work directory for per-request render jobs.

## Required Environment

- `SLACK_BOT_TOKEN` or `SLACK_BOT_TOKEN_FILE`
- `SLACK_SIGNING_SECRET` or `SLACK_SIGNING_SECRET_FILE`

## Optional Environment

- `BIND_ADDR`, default `0.0.0.0:3000`
- `SATYSFI_BOT_COMMAND`, default `satysfi`
- `SATYSFI_BOT_PDFTOPPM_COMMAND`, default `pdftoppm`
- `SATYSFI_BOT_CURL_COMMAND`, default `curl`
- `SATYSFI_BOT_WORK_DIR`, default `/tmp/satysfi-slack-bot`
- `SATYSFI_BOT_RENDER_TIMEOUT_SECS`, default `45`
- `SATYSFI_BOT_MAX_PAGES`, default `5`
- `SATYSFI_BOT_MAX_IMPORT_FILE_BYTES`, default `1048576`
- `SATYSFI_BOT_LOG`, default `info`; set `debug` for detailed logs

## Compose Shape

```yaml
services:
  satysfi-slack-bot:
    build: .
    ports:
      - "3000:3000"
    environment:
      BIND_ADDR: 0.0.0.0:3000
      SATYSFI_BOT_WORK_DIR: /bot/work
    secrets:
      - slack_bot_token
      - slack_signing_secret

secrets:
  slack_bot_token:
    file: ./slack_bot_token.txt
  slack_signing_secret:
    file: ./slack_signing_secret.txt
```

Put the container behind HTTPS before registering it as a Slack Events API
Request URL.

## Safety Defaults

- Verify Slack signatures before decoding request bodies.
- Render in a fresh per-request directory.
- Enforce render timeouts and page limits.
- Upload generated artifacts in a thread under the original mention.
- Send sanitized SATySFi diagnostics as Slack code blocks.
