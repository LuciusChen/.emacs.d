#!/bin/bash

# https://tdlib.github.io/td/build.html
# Function to install packages based on the operating system
install_packages() {
  if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    if ! xcode-select -p &>/dev/null; then
      xcode-select --install
      # Run xcodebuild -runFirstLaunch to complete setup
      sudo xcodebuild -runFirstLaunch
    fi

    brew list gperf &>/dev/null || brew install gperf
    brew list cmake &>/dev/null || brew install cmake
    brew list openssl &>/dev/null || brew install openssl

  elif [[ -n "$(command -v pacman)" ]]; then
    # Arch Linux
    sudo pacman -Sy --needed gperf cmake openssl
  else
    echo "Unsupported OS or missing package manager."
    exit 1
  fi
}

install_packages

# Set the target directory for cloning the repository
TARGET_DIR=~/td

# Clone or update the tdlib repository
if [ ! -d "$TARGET_DIR" ]; then
  git clone https://github.com/tdlib/td.git "$TARGET_DIR"
  cd "$TARGET_DIR"
else
  cd "$TARGET_DIR"
  git fetch
fi

BUILT_COMMIT_FILE="$TARGET_DIR/.built_commit"
SYSTEM_BUILT_COMMIT_FILE="$TARGET_DIR/.built_commit_usr_local"

# If a commit hash is provided as an argument, checkout that commit
if [ -n "$1" ]; then
  git checkout "$1"
else
  git checkout master
  git pull
  LOCAL=$(git rev-parse HEAD)
  BUILT=$(cat "$BUILT_COMMIT_FILE" 2>/dev/null)
  SYSTEM_BUILT=$(cat "$SYSTEM_BUILT_COMMIT_FILE" 2>/dev/null)

  if [[ "$OSTYPE" == "darwin"* ]]; then
    UP_TO_DATE=true
  else
    UP_TO_DATE=$([ "$LOCAL" = "$SYSTEM_BUILT" ] && echo true || echo false)
  fi

  if [ "$LOCAL" = "$BUILT" ] && [ "$UP_TO_DATE" = true ]; then
    echo "Already up-to-date and successfully built. No build needed."
    exit 0
  fi
fi

# Request sudo access only if build is required
sudo -v

# Keep-alive: update existing `sudo` time stamp until `.sh` has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Build tdlib
rm -rf build
mkdir build
cd build

# Set CMake command based on OS
if [[ "$OSTYPE" == "darwin"* ]]; then
  cmake -DCMAKE_BUILD_TYPE=Release -DOPENSSL_ROOT_DIR=/usr/local/opt/openssl/ -DCMAKE_INSTALL_PREFIX:PATH=/usr/local ..
  sudo cmake --build . --target install && git rev-parse HEAD > "$BUILT_COMMIT_FILE"
else
  cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX:PATH=../tdlib ..
  cmake --build . --target install

  # Keep /usr/local in sync for consumers like telega that link against it.
  sudo cmake --install . --prefix /usr/local
  git rev-parse HEAD > "$BUILT_COMMIT_FILE"
  git rev-parse HEAD > "$SYSTEM_BUILT_COMMIT_FILE"
fi
