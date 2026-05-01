# SATySFi Slack Bot

An OCaml Slack bot that renders SATySFi code when mentioned.

Use it in Slack as:

```text
@satysfi-bot render `hello, world!`
```

````text
@satysfi-bot r ```@require: stdja

document (|
  title = {Example};
  author = {};
  show-title = false;
  show-toc = false;
|) '<
  +p {Full document}
>
```
````

The bot listens for Slack Events API `app_mention` events at `POST /slack/events`.
It verifies Slack signatures, acknowledges events quickly, renders with the
`satysfi` executable, converts PDFs to PNG previews with `pdftoppm`, and uploads
files using Slack's current external upload flow.

## Commands

- `@satysfi-bot render <code block>`
- `@satysfi-bot r <code block>`
- `@satysfi-bot help [topic]`
- `@satysfi-bot version`
- `@satysfi-bot source`

Rendered output is uploaded as PNG page images.

Inline snippets are wrapped in a minimal `stdja` document. If the code already
looks like a full SATySFi document, it is passed through as-is.

## Hosting

Required environment variables:

- `SLACK_BOT_TOKEN` or `SLACK_BOT_TOKEN_FILE`
- `SLACK_SIGNING_SECRET` or `SLACK_SIGNING_SECRET_FILE`

Optional environment variables:

- `BIND_ADDR`, default `0.0.0.0:3000`
- `SATYSFI_BOT_WORK_DIR`, default `/tmp/satysfi-slack-bot`
- `SATYSFI_BOT_COMMAND`, default `satysfi`
- `SATYSFI_BOT_CONFIG_PATHS`, optional colon-separated paths passed to `satysfi -C`
- `SATYSFI_BOT_PDFTOPPM_COMMAND`, default `pdftoppm`
- `SATYSFI_BOT_CURL_COMMAND`, default `curl`
- `SATYSFI_BOT_MAX_BODY_BYTES`, default `1048576`
- `SATYSFI_BOT_MAX_IMPORT_FILE_BYTES`, default `1048576`
- `SATYSFI_BOT_MAX_PAGES`, default `5`
- `SATYSFI_BOT_RENDER_TIMEOUT_SECS`, default `45`
- `SATYSFI_BOT_SOURCE_URL`
- `SATYSFI_BOT_LOG`, default `info`; set `debug` for detailed request/API logs

Slack app setup:

- Enable Event Subscriptions and set the Request URL to
  `https://your-public-host.example/slack/events`.
- Subscribe to the `app_mention` bot event.
- Add bot token scopes: `app_mentions:read`, `chat:write`, `files:read`, `files:write`.

To use local SATySFi imports or assets, attach support files to the same message
as the `render` command. The bot downloads `.satyh`, `.satyg`, `.png`, `.jpg`,
`.jpeg`, `.pdf`, and `.svg` files into the render job directory, so code such as
`@import: my-lib` can load an attached `my-lib.satyh`, and image references can
use an attached `satysfi-logo.jpg`.

Run locally:

```sh
opam install . --deps-only --with-test
dune exec satysfi-slack-bot
```

## Development

Run the test suite with:

```sh
dune runtest
```

The tests live under `test/` and cover request verification, command parsing,
Slack-safe diagnostics, and renderer behavior using fake `satysfi`/`pdftoppm`
commands. They do not contact Slack.

## Docker

See [docs/docker.md](docs/docker.md) for the container layout.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
