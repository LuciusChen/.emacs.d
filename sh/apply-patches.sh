#!/bin/bash

# uninstall first
brew uninstall emacs-plus@31

# Navigate to the repository and pull the latest changes
cd /opt/homebrew/Library/Taps/d12frosted/homebrew-emacs-plus || exit
git pull

# Define the path to the formula and patches
FORMULA_PATH="/opt/homebrew/Library/Taps/d12frosted/homebrew-emacs-plus/Formula/emacs-plus@31.rb"
PATCH_DIR="$HOME/.emacs.d/patches"
TARGET_PATCH_DIR="/opt/homebrew/Library/Taps/d12frosted/homebrew-emacs-plus/patches/emacs-31"

# Calculate SHA for patches
SHA_ALPHA=$(shasum -a 256 "$PATCH_DIR/ns-alpha-background.patch" | awk '{ print $1 }')
SHA_INPUT=$(shasum -a 256 "$PATCH_DIR/ns-mac-input-source.patch" | awk '{ print $1 }')

# Copy the original formula for backup
cp "$FORMULA_PATH" "${FORMULA_PATH}.bak"

# Define the insertion point in the formula
INSERTION_POINT="round-undecorated-frame"

# Inject local_patch lines into the formula
sed -i '' "/$INSERTION_POINT/a\\
  local_patch \"ns-alpha-background\", sha: \"$SHA_ALPHA\"\\
  local_patch \"ns-mac-input-source\", sha: \"$SHA_INPUT\"
" "$FORMULA_PATH"

# Create symbolic links instead of copying the patch files
ln -sf "$PATCH_DIR/ns-alpha-background.patch" "$TARGET_PATCH_DIR/ns-alpha-background.patch"
ln -sf "$PATCH_DIR/ns-mac-input-source.patch" "$TARGET_PATCH_DIR/ns-mac-input-source.patch"

echo "Patches applied successfully."

# Install emacs-plus@31 with specified options
brew install emacs-plus@31 --with-savchenkovaleriy-big-sur-icon --with-xwidgets
