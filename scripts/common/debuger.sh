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
# add `-U`' to force update.
# [ERROR] Failed to resolve target definition file:/home/lucius/.emacs.d/debug-adapters/java-debug/com.microsoft.java.debug.target/com.microsoft.java.debug.tp.target: Failed to load p2 metadata repository from location https://download.eclipse.org/eclipse/updates/4.36-I-builds/: No repository found at https://download.eclipse.org/eclipse/updates/4.36-I-builds.
# 出现以上错误的时候可以直接查找对应的地址在浏览器是否可以找到对应的 repository，可以尝试删除 I-builds。
./mvnw clean install
