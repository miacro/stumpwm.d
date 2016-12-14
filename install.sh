#!/bin/sh
sbcl --eval "(ql:quickload :clx)" --quit
sbcl --eval "(ql:quickload :cl-ppcre)" --quit
sbcl --eval "(ql:quickload :xembed)" --quit
sbcl --eval "(ql:quickload  :alexandria)" --quit

cache_dir=${HOME}/.stumpwm.d/.cache
cache_stumpwm_dir=${cache_dir}/stumpwm
modules_path=${HOME}/.stumpwm.d/modules
contrib_path=${modules_path}/stumpwm-contrib
mkdir -p ${cache_dir}
if [[ -d ${cache_stumpwm_dir}/.git ]]
then
  cd ${cache_stumpwm_dir} && git pull
else
  git clone https://github.com/stumpwm/stumpwm ${cache_stumpwm_dir}
fi

cd ${cache_stumpwm_dir}
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
