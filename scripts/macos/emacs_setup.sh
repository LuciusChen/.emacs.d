#!/bin/bash

# Function to print usage
usage() {
  echo "Usage: $0 [HOMEBREW_EMACS_PLUS_31_REVISION=<commit>] [normal|mps] [30|31]"
  echo "  normal: Install the normal version (default)"
  echo "  mps: Install the MPS version"
  echo "  30: Install emacs-plus@30"
  echo "  31: Install emacs-plus@31 (default)"
  exit 1
}

# Set default version type and emacs version
VERSION_TYPE="normal"
EMACS_VERSION="31"
REVISION=""

# Process arguments
for arg in "$@"; do
  case $arg in
    HOMEBREW_EMACS_PLUS_31_REVISION=*)
      REVISION="${arg#*=}"
      shift
      ;;
    normal|mps)
      VERSION_TYPE=$arg
      shift
      ;;
    30|31)
      EMACS_VERSION=$arg
      shift
      ;;
    *)
      usage
      ;;
  esac
done

# Uninstall first
if brew list --versions emacs-plus@$EMACS_VERSION > /dev/null 2>&1; then
  echo "Uninstalling existing emacs-plus@$EMACS_VERSION..."
  brew uninstall emacs-plus@$EMACS_VERSION
else
  echo "emacs-plus@$EMACS_VERSION is not installed. Skipping uninstall step."
fi

# Navigate to the repository and pull the latest changes
cd /opt/homebrew/Library/Taps/d12frosted/homebrew-emacs-plus || exit
git pull

# Define the path to the formula and patches
FORMULA_PATH="/opt/homebrew/Library/Taps/d12frosted/homebrew-emacs-plus/Formula/emacs-plus@$EMACS_VERSION.rb"
PATCH_DIR="$HOME/.emacs.d/scripts/macos/patches/emacs-$EMACS_VERSION"
TARGET_PATCH_DIR="/opt/homebrew/Library/Taps/d12frosted/homebrew-emacs-plus/patches/emacs-$EMACS_VERSION"

# Copy the original formula for backup
cp "$FORMULA_PATH" "${FORMULA_PATH}.bak"

# Modify the formula based on the version type
if [ "$VERSION_TYPE" = "mps" ]; then
  echo "Switching to MPS version on feature/igc branch."

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

  # Update the formula for MPS
  sed -i '' 's|:branch => "master"|:branch => "feature/igc"|' "$FORMULA_PATH"
  sed -i '' '/args << "--with-gnutls"/a\
    args << "--with-mps=yes"
' "$FORMULA_PATH"

  # Add dependency on automake
  echo "Adding automake dependency for MPS version."
  sed -i '' '/depends_on.*"pkg-config".*=>.*:build/a\
  depends_on "automake" => :build
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
  local excluded_patches=()
  local exclude_file="$PATCH_DIR/exclude.txt"

  # Read excluded patches from exclude.txt (if exists)
  if [ -f "$exclude_file" ]; then
    while IFS= read -r line || [ -n "$line" ]; do
      # Skip empty lines and comments
      [[ -z "$line" || "$line" =~ ^# ]] && continue
      excluded_patches+=("$line")
    done < "$exclude_file"
    echo "Excluded patches: ${excluded_patches[*]}"
  fi

  # Remove excluded patches from formula
  for patch_name in "${excluded_patches[@]}"; do
    if grep -q "local_patch \"$patch_name\"" "$FORMULA_PATH"; then
      sed -i '' "/local_patch \"$patch_name\"/d" "$FORMULA_PATH"
      echo "Excluded patch $patch_name from the formula."
      rm -f "$TARGET_PATCH_DIR/$patch_name.patch"
    fi
  done

  # Read existing user patches from the formula
  local existing_user_patches=()
  while read -r line; do
    if [[ $line =~ local_patch\ \"([^\"]+)\".*#\ user_patch ]]; then
      existing_user_patches+=("${BASH_REMATCH[1]}")
    fi
  done < <(grep "local_patch" "$FORMULA_PATH")

  # Remove user patches that no longer exist in patch directory
  for old_patch in "${existing_user_patches[@]}"; do
    if [ ! -f "$PATCH_DIR/$old_patch.patch" ]; then
      sed -i '' "/local_patch \"$old_patch\".*# user_patch/d" "$FORMULA_PATH"
      echo "Removed user patch $old_patch from the formula."
      rm -f "$TARGET_PATCH_DIR/$old_patch.patch"
    fi
  done

  # Process user patches
  for patch in "$PATCH_DIR"/*.patch; do
    [ -f "$patch" ] || continue

    local patch_name
    patch_name=$(basename "$patch" .patch)
    local sha
    sha=$(shasum -a 256 "$patch" | awk '{ print $1 }')

    if grep -q "local_patch \"$patch_name\"" "$FORMULA_PATH" && ! grep -q "local_patch \"$patch_name\".*# user_patch" "$FORMULA_PATH"; then
      # Update SHA for formula patches that user is overriding
      sed -i '' "s|local_patch \"$patch_name\", sha: \"[^\"]*\"|local_patch \"$patch_name\", sha: \"$sha\"|" "$FORMULA_PATH"
      echo "Updated SHA for existing patch $patch_name."
    elif ! grep -q "local_patch \"$patch_name\"" "$FORMULA_PATH"; then
      # Add new user patch
      sed -i '' "/$INSERTION_POINT/a\\
  local_patch \"$patch_name\", sha: \"$sha\" # user_patch
" "$FORMULA_PATH"
      echo "Added patch $patch_name to the formula."
      inserted=true
    else
      echo "Patch $patch_name already exists."
    fi

    ln -sf "$patch" "$TARGET_PATCH_DIR/${patch_name}.patch"
  done

  if [ "$inserted" = true ]; then
    echo "New patches were added."
  else
    echo "All patches are up to date."
  fi
}

# Call the function to inject patches
inject_patches

# Install emacs-plus with specified options
if [ -n "$REVISION" ]; then
  HOMEBREW_EMACS_PLUS_31_REVISION=$REVISION brew install emacs-plus@$EMACS_VERSION --with-xwidgets
else
  brew install emacs-plus@$EMACS_VERSION --with-xwidgets
fi

osascript -e "tell application \"Finder\" to make alias file to posix file \"/opt/homebrew/opt/emacs-plus@$EMACS_VERSION/Emacs.app\" at posix file \"/Applications\" with properties {name:\"Emacs.app\"}"
