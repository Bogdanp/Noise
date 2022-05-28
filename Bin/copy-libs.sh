#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 2 ]; then
    echo "usage: $0 ARCH RACKET_PATH"
    exit 1
fi

ROOT="$(dirname "$0")/.."
ARCH="$1"
RACKET="$2"

cp "$RACKET/racket/include/chezscheme.h" "$ROOT/Lib/include/chezscheme-$ARCH.h"
cp "$RACKET/racket/include/racketcsboot.h" "$ROOT/Lib/include/racketcsboot.h"
cp "$RACKET/racket/include/racketcs.h" "$ROOT/Lib/include/racketcs.h"
cp "$RACKET/racket/lib/libracketcs.a" "$ROOT/Lib/libracketcs-$ARCH.a"

mkdir -p "$ROOT/Sources/Noise/boot/$ARCH"
cp "$RACKET/racket/lib/petite.boot" "$ROOT/Sources/Noise/boot/$ARCH/petite.boot"
cp "$RACKET/racket/lib/scheme.boot" "$ROOT/Sources/Noise/boot/$ARCH/scheme.boot"
cp "$RACKET/racket/lib/racket.boot" "$ROOT/Sources/Noise/boot/$ARCH/racket.boot"
