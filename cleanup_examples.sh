#!/bin/bash

# cleanup_examples.sh
# Script to clean up original files from the examples directory
# while preserving files referenced in docs and new subdirectories

# Exit on any error
set -e

# Set working directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Important files to preserve (referenced in docs)
PRESERVE_FILES=(
  "tuple_operations.m28"
)

# Check if math_utils.m28 exists (referenced in docs)
if [ -f "examples/math_utils.m28" ]; then
  PRESERVE_FILES+=("math_utils.m28")
fi

echo "=== Example Files Cleanup ==="
echo "This script will remove original files from the examples directory"
echo "that have been reorganized into subdirectories."
echo ""
echo "Files that will be preserved:"
for file in "${PRESERVE_FILES[@]}"; do
  echo "  - $file"
done
echo ""
echo "WARNING: This operation cannot be undone. Make sure you have a backup."
echo "         or commit your changes before proceeding."
echo ""
# Check if running in non-interactive mode
if [ -z "$NONINTERACTIVE" ]; then
  read -p "Are you sure you want to continue? (y/N): " confirm
  if [[ "$confirm" != "y" && "$confirm" != "Y" ]]; then
    echo "Operation cancelled."
    exit 0
  fi
else
  echo "Running in non-interactive mode, proceeding with cleanup."
fi

# Get list of original files (only files in the root examples dir, no subdirs)
cd examples
EXAMPLE_FILES=$(find . -maxdepth 1 -type f -name "*.m28")

# Files deleted counter
deleted=0

# Process each file
for file in $EXAMPLE_FILES; do
  filename=$(basename "$file")
  
  # Skip preserved files
  skip=false
  for preserve in "${PRESERVE_FILES[@]}"; do
    if [ "$filename" == "$preserve" ]; then
      skip=true
      break
    fi
  done
  
  if [ "$skip" == true ]; then
    echo "Preserving $filename (referenced in docs)"
    continue
  fi
  
  echo "Removing $filename"
  rm "$file"
  deleted=$((deleted + 1))
done

echo ""
echo "Cleanup completed. Removed $deleted files."
echo "Preserved ${#PRESERVE_FILES[@]} important files."