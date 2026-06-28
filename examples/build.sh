#!/bin/bash
# Auto-generated build script for READPEOPL

set -e  # Exit on error

echo "Building READPEOPL..."
gcc -o READPEOPL READPEOPL.c rpg_runtime.c rpg_file.c rpg_data.c -lm -lsqlite3 -O2
echo "Build successful: READPEOPL"
echo "Usage: ./READPEOPL <input_files>"
