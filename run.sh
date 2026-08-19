#!/usr/bin/env bash
# Run the game server on http://127.0.0.1:8000
set -euo pipefail

cd "$(dirname "$0")"
if [ ! -x .build/game8885 ]; then
    ./build.sh
fi
exec ./.build/game8885
