#!/bin/sh
modules_path=${HOME}/.stumpwm.d/modules
contrib_path=${HOME}/.stumpwm.d/modules/stumpwm-contrib
sbcl --eval "(ql:quickload :clx)" --quit
sbcl --eval "(ql:quickload :cl-ppcre)" --quit
autoconf
./configure --prefix=${HOME}/.stumpwm.d --with-module-dir=${HOME}/.stumpwm.d/modules
make && make install 
mkdir -p ${modules_path}
cd ${modules_path}
if [[ -d ${contrib_path}/.git ]] 
then
  cd ${contrib_path} && git pull 
else
  git clone https://github.com/stumpwm/stumpwm-contrib.git ${contrib_path}
fi
