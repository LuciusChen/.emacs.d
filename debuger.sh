#!/bin/bash
# Use `curl` to get the JSON response for the latest release
# Use `grep` to find the line containing file URL
# Use `cut` and `tr` to extract the URL
# Use `wget` to download it
curl -s https://api.github.com/repos/microsoft/vscode-js-debug/releases/latest \
| grep "browser_download_url.*gz" \
| cut -d : -f 2,3 \
| tr -d \" \
| wget -O latest-vscode-js-debug.tar.gz -qi -

mkdir -p ~/.emacs.d/debug-adapters
tar -xvzf latest-vscode-js-debug.tar.gz -C ~/.emacs.d/debug-adapters
rm latest-vscode-js-debug.tar.gz

cd ~/.emacs.d/debug-adapters
git clone https://github.com/microsoft/java-debug.git
cd java-debug
./mvnw clean install
