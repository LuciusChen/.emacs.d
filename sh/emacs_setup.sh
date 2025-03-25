#!/bin/bash

# Function to print usage
usage() {
  echo "Usage: $0 [normal|mps]"
  echo "  normal: Install the normal version of emacs-plus@31 (default)"
  echo "  mps: Install the MPS version of emacs-plus@31"
  exit 1
}

# Set default version type
VERSION_TYPE="normal"

# Check if the user provided an argument and if it's valid
if [ -n "$1" ]; then
  if [ "$1" = "normal" ] || [ "$1" = "mps" ]; then
    VERSION_TYPE=$1
  else
    usage
  fi
fi

# Uninstall first
if brew list --versions emacs-plus@31 > /dev/null 2>&1; then
  echo "Uninstalling existing emacs-plus@31..."
  brew uninstall emacs-plus@31
else
  echo "emacs-plus@31 is not installed. Skipping uninstall step."
fi

# Navigate to the repository and pull the latest changes
cd /opt/homebrew/Library/Taps/d12frosted/homebrew-emacs-plus || exit
git pull

# Define the path to the formula and patches
FORMULA_PATH="/opt/homebrew/Library/Taps/d12frosted/homebrew-emacs-plus/Formula/emacs-plus@31.rb"
PATCH_DIR="$HOME/.emacs.d/patches"
TARGET_PATCH_DIR="/opt/homebrew/Library/Taps/d12frosted/homebrew-emacs-plus/patches/emacs-31"

# Copy the original formula for backup
cp "$FORMULA_PATH" "${FORMULA_PATH}.bak"

# Modify the formula based on the version type
if [ "$VERSION_TYPE" = "mps" ]; then
  echo "Switching to MPS version."

  # Check if Xcode is installed
  if ! xcode-select -p > /dev/null 2>&1; then
      echo "Xcode is not installed. Please install Xcode from the App Store."
      exit 1
  fi

  # Check if Xcode initial setup is required
  if ! xcodebuild -checkFirstLaunchStatus > /dev/null 2>&1; then
      echo "Running Xcode first launch setup..."
      sudo xcodebuild -runFirstLaunch
  fi

  # Ensure libmps is installed
  if ! brew list --versions libmps > /dev/null 2>&1; then
    brew install libmps --HEAD
  else
    echo "libmps is already installed."
  fi

  # Update the formula for MPS
  sed -i '' 's|:branch => "master"|:branch => "feature/igc"|' "$FORMULA_PATH"
  sed -i '' '/args << "--with-gnutls"/a\
    args << "--with-mps=yes"
' "$FORMULA_PATH"
else
  echo "Switching to normal version."
  # Revert any changes for MPS if needed
  git checkout "$FORMULA_PATH"
fi

# Define the insertion point in the formula
INSERTION_POINT="round-undecorated-frame"

# Function to calculate SHA and inject patch
inject_patches() {
  local inserted=false
  local existing_user_patches=()
  local existing_non_user_patches=()

  # Read all existing user-defined patches from the formula
  while read -r line; do
    if [[ $line =~ local_patch\ \"([^\"]+)\".*#\ user_patch ]]; then
      existing_user_patches+=("${BASH_REMATCH[1]}")
    fi
  done < <(grep "local_patch" "$FORMULA_PATH")

  # Read all existing non-user-defined patches from the formula
  while read -r line; do
    if [[ $line =~ local_patch\ \"([^\"]+)\" ]] && [[ ! $line =~ \#\ user_patch ]]; then
      existing_non_user_patches+=("${BASH_REMATCH[1]}")
    fi
  done < <(grep "local_patch" "$FORMULA_PATH")

  # Remove entries for user patches that no longer exist
  for old_patch in "${existing_user_patches[@]}"; do
    if [ ! -f "$PATCH_DIR/$old_patch.patch" ]; then
      sed -i '' "/local_patch \"$old_patch\".*# user_patch/d" "$FORMULA_PATH"
      echo "Removed user patch $old_patch from the formula."
      rm -f "$TARGET_PATCH_DIR/$old_patch.patch"
      echo "Removed symbolic link for $old_patch."
    fi
  done

  # Process new user patches
  for patch in "$PATCH_DIR"/*.patch; do
    local patch_name
    patch_name=$(basename "$patch" .patch)
    local sha
    sha=$(shasum -a 256 "$patch" | awk '{ print $1 }')

    if grep -q "local_patch \"$patch_name\"" "$FORMULA_PATH" && ! grep -q "local_patch \"$patch_name\".*# user_patch" "$FORMULA_PATH"; then
      # Update SHA for non-user patches if they exist in user patches
      sed -i '' "s|local_patch \"$patch_name\", sha: \".*\"|local_patch \"$patch_name\", sha: \"$sha\"|" "$FORMULA_PATH"
      echo "Updated SHA for existing non-user patch $patch_name in the formula."
    elif ! grep -q "local_patch \"$patch_name\"" "$FORMULA_PATH"; then
      sed -i '' "/$INSERTION_POINT/a\\
  local_patch \"$patch_name\", sha: \"$sha\" # user_patch
" "$FORMULA_PATH"
      echo "Patch $patch_name added to the formula."
      inserted=true
    else
      echo "Patch $patch_name already exists in the formula."
    fi

    ln -sf "$patch" "$TARGET_PATCH_DIR/${patch_name}.patch"
  done

  if [ "$inserted" = true ]; then
    echo "All patches applied successfully."
  else
    echo "No new patches to apply."
  fi
}

# Call the function to inject patches
inject_patches

# Install emacs-plus@31 with specified options
brew install emacs-plus@31 --with-savchenkovaleriy-big-sur-icon --with-xwidgets

osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@31/Emacs.app" at posix file "/Applications" with properties {name:"Emacs.app"}'
