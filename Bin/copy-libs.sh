#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 2 ]; then
    echo "usage: $0 ARCH RACKET_PATH"
    exit 1
fi

ROOT="$(dirname "$0")/.."
ARCH="$1"
RACKET="$2"

cp "$RACKET/include/chezscheme.h" "$ROOT/Lib/include/chezscheme-$ARCH.h"
cp "$RACKET/include/racketcsboot.h" "$ROOT/Lib/include/racketcsboot.h"
cp "$RACKET/include/racketcs.h" "$ROOT/Lib/include/racketcs.h"
cp "$RACKET/lib/libracketcs.a" "$ROOT/Lib/libracketcs-$ARCH.a"

# For iPhoneSimulator, we always use the iOS bootfiles so there's no
# point in copying them.
if [ "$ARCH" != "arm64-iphonesimulator" ]; then
    mkdir -p "$ROOT/boot/$ARCH"
    cp "$RACKET/lib/petite.boot" "$ROOT/boot/$ARCH/petite.boot"
    cp "$RACKET/lib/scheme.boot" "$ROOT/boot/$ARCH/scheme.boot"
    cp "$RACKET/lib/racket.boot" "$ROOT/boot/$ARCH/racket.boot"
fi
