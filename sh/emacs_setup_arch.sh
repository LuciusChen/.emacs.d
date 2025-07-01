#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Change to the directory containing the Emacs source code
cd ~/emacs && git pull

# Run autogen.sh to prepare the build system
./autogen.sh

# Configure the build with specific options
./configure \
--with-dbus \
--with-gif \
--with-jpeg \
--with-png \
--with-rsvg \
--with-tiff \
--with-xft \
--with-xpm \
--with-gpm=no \
--with-imagemagick \
--with-modules \
--with-native-compilation=aot \
--with-pgtk \
--with-tree-sitter \
--without-pop \
--prefix=/usr/local

# Compile the project using all available CPU cores
make -j$(nproc)

# Install the compiled binaries and other files
sudo make install
