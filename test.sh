#!/usr/bin/env bash
# Build and run the test suite.
set -euo pipefail

GHC_BIN="${GHC_BIN:-$HOME/.stack/programs/x86_64-linux/ghc-tinfo6-libc6-pre232-9.6.4/bin/ghc}"
PKGDB="${PKGDB:-$HOME/.stack/snapshots/x86_64-linux-tinfo6-libc6-pre232/2ffcbd0a44e5bf2238ee3c1f5a61323b01b5e0f4d57520d2c7f8cf24fc62b261/9.6.4/pkgdb}"

mkdir -p .build

"$GHC_BIN" -O1 -Wall -threaded -rtsopts \
    -package-db "$PKGDB" -isrc src/TestMain.hs -o .build/game8885-test

exec ./.build/game8885-test
