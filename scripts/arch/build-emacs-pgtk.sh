#!/bin/bash

# Define paths
EMACS_D="$HOME/.emacs.d/scripts/arch/emacs-pgtk-igc"
BUILD_DIR="$HOME/emacs-pgtk-igc"

# Create the build directory if it doesn't exist
mkdir -p "$BUILD_DIR"

# Copy PKGBUILD to the build directory
cp "$EMACS_D/PKGBUILD" "$BUILD_DIR/"
cp "$EMACS_D"/*.patch "$BUILD_DIR/"

# Change to the build directory
cd "$BUILD_DIR"

# Run makepkg to build and install the package
makepkg -si
