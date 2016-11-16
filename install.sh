#!/bin/sh
sbcl --eval "(ql:quickload :clx)" --quit
sbcl --eval "(ql:quickload :cl-ppcre)" --quit
autoconf
./configure --prefix=${HOME}/.stumpwm.d
make && make install && [[ -d ${HOME}/.stumpwm.d/modules/.git ]] || make install-modules
