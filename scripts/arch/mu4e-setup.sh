#!/bin/bash

yay -Syu mu isync msmtp

cp ~/dotfiles/arch/.mbsyncrc ~/
cp ~/dotfiles/arch/.msmtprc ~/

mkdir -p ~/.maildir/qq ~/.maildir/gmail ~/.maildir/163

mbsync -aV

mu init -m ~/.maildir --my-address chenyh1013@163.com --my-address chenyh572@gmail.com --my-address chenyaohua@njcjh.cn
mu index
