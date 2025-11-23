#!/bin/bash
# Script to help fix LocatedValue unwrapping issues
# Usage: ./scripts/fix_unwrap.sh <file> <line_number>

FILE=$1
LINE=$2

if [ -z "$FILE" ] || [ -z "$LINE" ]; then
    echo "Usage: $0 <file> <line_number>"
    exit 1
fi

echo "Showing context around line $LINE in $FILE:"
sed -n "$((LINE-2)),$((LINE+2))p" "$FILE" | nl -v $((LINE-2))
