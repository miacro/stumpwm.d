#!/bin/sh
module_path=${HOME}/.stumpwm.d/modules/stumpwm-contrib
sbcl --eval "(ql:quickload :clx)" --quit
sbcl --eval "(ql:quickload :cl-ppcre)" --quit
autoconf
./configure --prefix=${HOME}/.stumpwm.d --with-module-dir=${module_path}
make && make install && [[ -d ${module_path}/.git ]] || make install-modules
