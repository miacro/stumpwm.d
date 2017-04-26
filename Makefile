MAKE=make --no-print-directory
SHELL=/bin/bash

SOURCE_DIR=`pwd`
TARGET_DIR=${HOME}/.stumpwm.d
CACHE_DIR=${TARGET_DIR}/.cache
MODULE_DIR=${TARGET_DIR}/modules
REPO_URL=
REPO_DIR=

relink:
	@  ${MAKE} unlink \
  && ${MAKE} link

link:
	@  [[ ! -L ${TARGET_DIR} ]] && [[ ! -f ${TARGET_DIR} ]] \
  && ln -s ${SOURCE_DIR} ${TARGET_DIR} \
  ||  exit 0

unlink:
	@  [[ -L ${TARGET_DIR} ]] \
  && rm ${TARGET_DIR} \
  || exit 0

install:
	make prepare-libs \
	&& make prepare-repo REPO_URL=https://github.com/stumpwm/stumpwm REPO_DIR=${CACHE_DIR}/stumpwm \
  && cd ${CACHE_DIR}/stumpwm \
  && autoconf \
  && ./configure --prefix=${TARGET_DIR} --with-module-dir=${MODULE_DIR} \
  && make \
  && make install \
  && cd - \
  && make install-contrib

install-contrib:
	@  mkdir -p ${MODULE_DIR} \
  && make prepare-repo REPO_URL=https://github.com/stumpwm/stumpwm-contrib.git REPO_DIR=${MODULE_DIR}/stumpwm-contrib

prepare-libs:
	sbcl --eval "(ql:quickload :clx)" --quit \
  && sbcl --eval "(ql:quickload :cl-ppcre)" --quit \
  && sbcl --eval "(ql:quickload :xembed)" --quit \
  && sbcl --eval "(ql:quickload :alexandria)" --quit \
  && sbcl --eval "(ql:quickload :clx-truetype)" --quit

prepare-repo:
	@  [[ -d ${REPO_DIR}/.git ]] \
  && cd ${REPO_DIR} \
  && git pull \
  && cd - \
  || git clone ${REPO_URL} ${REPO_DIR}

.PHONY: relink unlink link prepare-libs prepare-repo install install-contrib