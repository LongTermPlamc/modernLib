#!/bin/bash

# Set variables
SRC_DIR="src"
BUILD_DIR="build"
FC="caf"  # Or gfortran, if you are not using coarrays

# Clean and recreate build directory
echo "Cleaning previous build..."
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# Compile each source file
echo "Compiling Fortran source files..."
for file in "$SRC_DIR"/*.f90; do
    echo "Compiling $file..."
    $FC -c "$file" -J "$BUILD_DIR" -o "$BUILD_DIR/$(basename "${file%.f90}.o")"
    if [ $? -ne 0 ]; then
        echo "❌ Compilation failed for $file"
        exit 1
    fi
done

# Create static library
echo "Creating static library libmodern.a..."
ar rcs "$BUILD_DIR/libmodern.a" "$BUILD_DIR"/*.o

echo "✅ Compilation completed. Files are in $BUILD_DIR/"