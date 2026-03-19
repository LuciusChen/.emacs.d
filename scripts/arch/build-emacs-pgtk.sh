#!/bin/bash

# Define paths
EMACS_D="$HOME/.emacs.d/scripts/arch/emacs-pgtk"
BUILD_DIR="$HOME/emacs-pgtk"

# Create the build directory if it doesn't exist
mkdir -p "$BUILD_DIR"

# Clean previous build artifacts so each run starts fresh
if compgen -G "$BUILD_DIR"/*.pkg.tar.* > /dev/null || [[ -d "$BUILD_DIR/src" || -d "$BUILD_DIR/pkg" ]]; then
  read -r -p "Remove old package files and build directories (src/ pkg/)? [y/N] " reply
  if [[ "$reply" =~ ^([yY]|[yY][eE][sS])$ ]]; then
    if compgen -G "$BUILD_DIR"/*.pkg.tar.* > /dev/null; then
      rm -f "$BUILD_DIR"/*.pkg.tar.*
      echo "Removed old package files in $BUILD_DIR"
    fi

    if [[ -d "$BUILD_DIR/src" || -d "$BUILD_DIR/pkg" ]]; then
      rm -rf "$BUILD_DIR/src" "$BUILD_DIR/pkg"
      echo "Removed old build directories: src/ pkg/"
    fi
  else
    echo "Skipped cleanup."
  fi
fi

# Copy PKGBUILD to the build directory
cp "$EMACS_D/PKGBUILD" "$BUILD_DIR/"

# Read branch from PKGBUILD (fallback to master)
BRANCH="$(sed -n 's/^_branch="\([^"]*\)".*/\1/p' "$BUILD_DIR/PKGBUILD" | head -n1)"
BRANCH="${BRANCH:-master}"

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

# Remove a stale working tree left behind by a previous failed makepkg run.
# makepkg expects $BUILD_DIR/src/emacs to be a git working copy for VCS
# sources; a plain directory there will make the next run fail with
# "not a git repository" during extraction.
if [[ -d "$BUILD_DIR/src/emacs" && ! -e "$BUILD_DIR/src/emacs/.git" ]]; then
  echo "Removing stale non-git working tree: $BUILD_DIR/src/emacs"
  rm -rf "$BUILD_DIR/src/emacs"
fi

# Update the cached bare repository that makepkg uses for the VCS source.
if git --git-dir="$BUILD_DIR/emacs" rev-parse --is-bare-repository >/dev/null 2>&1; then
  echo "Updating cached emacs git repo..."
  git --git-dir="$BUILD_DIR/emacs" fetch --all --prune
fi

# Show current directory structure
echo "Build directory structure:"
ls -la

# Run makepkg to build and install the package
makepkg -si
