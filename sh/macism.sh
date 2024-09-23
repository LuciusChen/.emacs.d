#!/bin/bash
brew tap laishulu/macism
brew install macism
cd ~
git clone https://github.com/LuciusChen/macism.git
cd macism
swiftc macism.swift
cp -rf macism /opt/homebrew/Cellar/macism/1.3.3/bin/
