#!/bin/sh

sbcl --eval "(require 'sshmenu)" --eval "(sb-ext:save-lisp-and-die \"/home/binarin/bin/sshmenu-cl\" :executable t :toplevel #'(lambda () (ru.binarin.sshmenu::click (ru.binarin.sshmenu::read-menu \"/home/binarin/.sshmenu-cl\"))))"
