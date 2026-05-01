FROM ocaml/opam:debian-12-ocaml-4.14

WORKDIR /src
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl poppler-utils m4 pkg-config ca-certificates \
    && rm -rf /var/lib/apt/lists/*
USER opam

COPY --chown=opam:opam dune-project satysfi-slack-bot.opam ./
RUN opam install . --deps-only --with-test -y

COPY --chown=opam:opam . .
RUN opam exec -- dune build @install

WORKDIR /bot
EXPOSE 3000

ENV BIND_ADDR=0.0.0.0:3000 \
    SATYSFI_BOT_WORK_DIR=/bot/work

USER root
RUN mkdir -p /bot/work && chown -R opam:opam /bot
USER opam

CMD ["opam", "exec", "--", "/src/_build/default/bin/main.exe"]
