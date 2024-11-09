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

case "$ARCH" in
    "arm64-ios")
        DEST="$ROOT/Sources/NoiseBoot_iOS/boot/$ARCH"
        mkdir -p "$DEST"
        cp "$RACKET/lib/petite.boot" "$DEST/petite.boot"
        cp "$RACKET/lib/scheme.boot" "$DEST/scheme.boot"
        cp "$RACKET/lib/racket.boot" "$DEST/racket.boot"
        ;;
    "arm64-macos" | "x86_64-macos")
        DEST="$ROOT/Sources/NoiseBoot_macOS/boot/$ARCH"
        mkdir -p "$DEST"
        cp "$RACKET/lib/petite.boot" "$DEST/petite.boot"
        cp "$RACKET/lib/scheme.boot" "$DEST/scheme.boot"
        cp "$RACKET/lib/racket.boot" "$DEST/racket.boot"
        ;;
    *)
        # For iPhoneSimulator, we use the iOS bootfiles so there's no
        # point in copying them.
        echo "warning: boot files ignored for $ARCH"
        ;;
esac
