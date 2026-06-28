#!/bin/bash
# Build all RPG samples

set -e

echo "Building all RPG samples..."
echo ""

# Process each .rpg and .RPG file
for rpg_file in *.rpg *.RPG; do
    [ -f "$rpg_file" ] || continue
    
    echo "Processing $rpg_file..."
    
    # Transpile
    ../RPG2C "$rpg_file"
    
    # Build if transpilation succeeded and build.sh was generated
    if [ -f "build.sh" ]; then
        ./build.sh
    fi
    
    echo ""
done

echo "All samples built successfully!"

# Made with Bob
