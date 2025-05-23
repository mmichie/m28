#!/bin/bash

# Find all Go files
GO_FILES=$(find . -name "*.go")

# Loop through each file
for file in $GO_FILES; do
    # Replace imports - use a more aggressive replacement
    if grep -q 'm28/' "$file"; then
        echo "Fixing imports in $file"
        # Replace imports
        sed -i '' 's|"m28/|"github.com/mmichie/m28/|g' "$file"
    fi
done

# Run go mod tidy to clean up dependencies
echo "Running go mod tidy..."
go mod tidy

echo "Import fixes completed."