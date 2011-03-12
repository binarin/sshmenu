#!/bin/sh
cl-launch --no-include -s sshmenu -d `pwd`/sshmenu-image -v -f ~/.sbclrc
cl-launch --no-include -m sshmenu-image -s sshmenu -v -f ~/.sbclrc -i '(ru.binarin.sshmenu:run)' -o sshmenu
