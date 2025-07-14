#!/bin/bash

# Install Command Line Tools if not already installed
if ! xcode-select -p &>/dev/null; then
  xcode-select --install
  # Run xcodebuild -runFirstLaunch to complete setup
  sudo xcodebuild -runFirstLaunch
fi

# Install necessary packages using Homebrew if not already installed
brew list gperf &>/dev/null || brew install gperf
brew list cmake &>/dev/null || brew install cmake
brew list openssl &>/dev/null || brew install openssl

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

# If a commit hash is provided as an argument, checkout that commit
if [ -n "$1" ]; then
  git checkout "$1"
else
  git checkout master
  LOCAL=$(git rev-parse HEAD)
  REMOTE=$(git rev-parse @{u})

  # Check if local and remote are the same
  if [ "$LOCAL" = "$REMOTE" ]; then
    echo "Already up-to-date. No build needed."
    exit 0
  fi

  git pull
fi

# Request sudo access only if build is required
sudo -v

# Keep-alive: update existing `sudo` time stamp until `.sh` has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Build tdlib
rm -rf build
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release -DOPENSSL_ROOT_DIR=/usr/local/opt/openssl/ -DCMAKE_INSTALL_PREFIX:PATH=/usr/local ..
sudo cmake --build . --target install
