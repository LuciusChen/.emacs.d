#!/bin/bash

# Define paths
EMACS_D="$HOME/.emacs.d/scripts/arch/emacs-pgtk-igc"
BUILD_DIR="$HOME/emacs-pgtk-igc"

# Create the build directory if it doesn't exist
mkdir -p "$BUILD_DIR"

# Copy PKGBUILD to the build directory
cp "$EMACS_D/PKGBUILD" "$BUILD_DIR/"

# Copy patches to build directory root if they exist
if [[ -d "$EMACS_D/patches" ]]; then
  # Remove old patch files
  rm -f "$BUILD_DIR"/*.patch

  # Copy all patch files to build root
  if compgen -G "$EMACS_D/patches/*.patch" > /dev/null; then
    cp "$EMACS_D/patches"/*.patch "$BUILD_DIR/"
    echo "Copied patches:"
    ls -1 "$BUILD_DIR"/*.patch
  else
    echo "No patch files found in $EMACS_D/patches/"
  fi
else
  echo "No patches directory found at $EMACS_D/patches/"
fi

# Change to the build directory
cd "$BUILD_DIR"

# Show current directory structure
echo "Build directory structure:"
ls -la

# Run makepkg to build and install the package
makepkg -si
