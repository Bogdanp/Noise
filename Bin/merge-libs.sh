#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -lt 3 ]; then
    echo "usage: $0 DEST A B"
    exit 1
fi

libtool -static -o "$1" "$2" "$3"
