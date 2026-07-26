#!/usr/bin/env bash
set -euo pipefail

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

    local package
    for package in gperf cmake openssl@3; do
      brew list "$package" &>/dev/null || brew install "$package"
    done

  elif [[ -n "$(command -v pacman)" ]]; then
    # Arch Linux
    local packages=(gperf cmake openssl ninja)
    local missing=()
    local package
    for package in "${packages[@]}"; do
      pacman -Q "$package" &>/dev/null || missing+=("$package")
    done
    if ((${#missing[@]})); then
      sudo pacman -S --needed "${missing[@]}"
    fi
  else
    echo "Unsupported OS or missing package manager."
    exit 1
  fi
}

# Set the target directory for cloning the repository
TARGET_DIR="${TDLIB_SOURCE_DIR:-$HOME/td}"
BUILD_DIR="$TARGET_DIR/build"
BUILD_JOBS="${TDLIB_BUILD_JOBS:-$(getconf _NPROCESSORS_ONLN)}"
REQUESTED_COMMIT="${1:-}"

# Clone or update the tdlib repository
if [ ! -d "$TARGET_DIR" ]; then
  git clone https://github.com/tdlib/td.git "$TARGET_DIR"
else
  git -C "$TARGET_DIR" fetch
fi

# If a commit hash is provided as an argument, checkout that commit
if [ -n "$REQUESTED_COMMIT" ]; then
  git -C "$TARGET_DIR" checkout "$REQUESTED_COMMIT"
else
  git -C "$TARGET_DIR" checkout master
  git -C "$TARGET_DIR" pull --ff-only
fi

install_packages

# Explicitly requested full rebuild: discard only TDLib's generated build tree.
if [ "${TDLIB_FORCE_REBUILD:-0}" = 1 ] && [ -d "$BUILD_DIR" ]; then
  echo "TDLIB_FORCE_REBUILD=1: removing the existing build directory..."
  rm -rf -- "$BUILD_DIR"
fi

CMAKE_ARGS=(
  -DCMAKE_BUILD_TYPE=Release
)
if [[ "$OSTYPE" == "darwin"* ]]; then
  CMAKE_GENERATOR="Unix Makefiles"
  CMAKE_ARGS+=(
    "-DOPENSSL_ROOT_DIR=${TDLIB_OPENSSL_ROOT_DIR:-$(brew --prefix openssl@3)}"
    -DCMAKE_INSTALL_PREFIX:PATH=/usr/local
  )
else
  CMAKE_GENERATOR="Ninja"
  CMAKE_ARGS+=("-DCMAKE_INSTALL_PREFIX:PATH=$TARGET_DIR/tdlib")
fi

# CMake generators cannot share a build tree.  Remove it once when switching
# between the macOS Makefiles build and the Ninja build used on Linux.
if [ -f "$BUILD_DIR/CMakeCache.txt" ] &&
  ! grep -Fqx "CMAKE_GENERATOR:INTERNAL=$CMAKE_GENERATOR" "$BUILD_DIR/CMakeCache.txt"; then
  echo "Switching the TDLib build directory to $CMAKE_GENERATOR..."
  rm -rf -- "$BUILD_DIR"
fi

# Configure and build TDLib.
cmake -S "$TARGET_DIR" -B "$BUILD_DIR" -G "$CMAKE_GENERATOR" "${CMAKE_ARGS[@]}"
cmake --build "$BUILD_DIR" --parallel "$BUILD_JOBS"
if [[ "$OSTYPE" != "darwin"* ]]; then
  cmake --install "$BUILD_DIR"
fi

# Keep /usr/local in sync for consumers like telega that link against it.
sudo cmake --install "$BUILD_DIR" --prefix /usr/local
