#!/bin/bash

brew install mu isync msmtp

cp ~/dotfiles/arch/home/.mbsyncrc ~/
cp ~/dotfiles/arch/home/.msmtprc ~/

mkdir -p ~/.maildir/qq ~/.maildir/gmail ~/.maildir/163

mbsync -aV

mu init -m ~/.maildir --my-address chenyh1013@163.com --my-address chenyh572@gmail.com --my-address chenyaohua@njcjh.cn
mu index
