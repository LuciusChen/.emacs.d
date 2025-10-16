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

# Set CMake command based on OS
if [[ "$OSTYPE" == "darwin"* ]]; then
  cmake -DCMAKE_BUILD_TYPE=Release -DOPENSSL_ROOT_DIR=/usr/local/opt/openssl/ -DCMAKE_INSTALL_PREFIX:PATH=/usr/local ..
else
  cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX:PATH=../tdlib ..
fi

sudo cmake --build . --target install
