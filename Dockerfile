# syntax=docker/dockerfile:1
FROM debian:trixie-slim@sha256:020c0d20b9880058cbe785a9db107156c3c75c2ac944a6aa7ab59f2add76a7bd AS builder

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

RUN --mount=type=cache,target=/var/lib/apt/lists \
    --mount=type=cache,target=/var/cache/apt \
    apt-get update && apt-get install -y --no-install-recommends \
    haskell-stack ca-certificates \
    gcc g++ make libc6-dev libgmp-dev \
    zlib1g-dev libncurses-dev

WORKDIR /gt

COPY stack.yaml stack.yaml.lock GridTactics.cabal .
RUN --mount=type=cache,target=/root/.stack \
    stack setup
RUN --mount=type=cache,target=/root/.stack \
    --mount=type=cache,target=/gt/.stack-work \
    stack build --dependencies-only

COPY . .
RUN --mount=type=cache,target=/root/.stack \
    --mount=type=cache,target=/gt/.stack-work \
    stack install --local-bin-path /out

FROM scratch AS artifact
COPY --from=builder /out/gt-client /out/gt-server /

FROM debian:trixie-slim@sha256:020c0d20b9880058cbe785a9db107156c3c75c2ac944a6aa7ab59f2add76a7bd
COPY --from=builder /out/gt-server /usr/local/bin/
COPY start-server.sh /usr/local/bin
RUN chmod +x /usr/local/bin/start-server.sh
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8
CMD ["/usr/local/bin/start-server.sh"]
