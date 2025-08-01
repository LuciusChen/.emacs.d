#!/bin/bash

# https://github.com/emacs-mirror/emacs/blob/feature/igc/README-IGC
MPS_ARTIFACTS="$HOME/mps/mps_artifacts"
BUILD_DIR="$HOME"
LIB_DEST="/usr/lib"
INCLUDE_DEST="/usr/include"
BIN_DEST="/usr/bin"

cd "$BUILD_DIR"
git clone https://github.com/Ravenbrook/mps.git
cd mps
autoconf
./configure --prefix="$MPS_ARTIFACTS"
make CFLAGSCOMPILERSTRICT="-Wno-error"
make install

# Copy files
if [ -d "$MPS_ARTIFACTS/lib" ]; then
  sudo cp -r "$MPS_ARTIFACTS/lib/"* "$LIB_DEST/"
fi

if [ -d "$MPS_ARTIFACTS/include" ]; then
  sudo cp -r "$MPS_ARTIFACTS/include/"* "$INCLUDE_DEST/"
fi

if [ -d "$MPS_ARTIFACTS/bin" ]; then
  sudo cp -r "$MPS_ARTIFACTS/bin/"* "$BIN_DEST/"
fi

echo "MPS installation and file copying completed successfully."
