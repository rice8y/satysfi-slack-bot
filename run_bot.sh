#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

if [ -f .env.local ]; then
  set -a
  # shellcheck disable=SC1091
  . ./.env.local
  set +a
fi

if [ -z "${SLACK_BOT_TOKEN:-}" ] && [ -z "${SLACK_BOT_TOKEN_FILE:-}" ]; then
  echo "Missing SLACK_BOT_TOKEN or SLACK_BOT_TOKEN_FILE." >&2
  echo "Put local secrets in .env.local; see .env.example." >&2
  exit 1
fi

if [ -z "${SLACK_SIGNING_SECRET:-}" ] && [ -z "${SLACK_SIGNING_SECRET_FILE:-}" ]; then
  echo "Missing SLACK_SIGNING_SECRET or SLACK_SIGNING_SECRET_FILE." >&2
  echo "Put local secrets in .env.local; see .env.example." >&2
  exit 1
fi

is_port_busy() {
  lsof -nP -iTCP:"$1" -sTCP:LISTEN >/dev/null 2>&1
}

if [ -z "${BIND_ADDR:-}" ]; then
  port=9999
  while is_port_busy "$port"; do
    port=$((port + 1))
  done
  export BIND_ADDR="127.0.0.1:${port}"
else
  port="${BIND_ADDR##*:}"
  if is_port_busy "$port"; then
    echo "Port ${port} is already in use:" >&2
    lsof -nP -iTCP:"$port" -sTCP:LISTEN >&2 || true
    echo "Set BIND_ADDR to another host:port, or stop the process above." >&2
    exit 1
  fi
fi

echo "Starting satysfi-slack-bot on ${BIND_ADDR}" >&2
echo "For trycloudflare, run: cloudflared tunnel --url http://${BIND_ADDR}" >&2
exec dune exec satysfi-slack-bot
