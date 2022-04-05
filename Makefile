MAKE=make --no-print-directory
SHELL=/bin/bash

SOURCE_DIR=$(realpath .)
TARGET_DIR=${HOME}/.stumpwm.d
CACHE_DIR=${TARGET_DIR}/.cache
MODULE_DIR=${TARGET_DIR}/modules
REPO_URL=
REPO_DIR=
OS=`uname -s`
QLLP="${HOME}/quicklisp/local-projects"

link:
	ln -fs -n ${SOURCE_DIR} ${TARGET_DIR}

unlink:
	[[ -L ${TARGET_DIR} ]] && rm ${TARGET_DIR} || exit 0

install-stumpwm:
	[[ ${OS} == "Linux" ]] \
	&& make prepare-libs \
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

prepare-libs: prepare-truetype
	sbcl --eval "(ql:quickload :clx)" --quit \
  && sbcl --eval "(ql:quickload :cl-ppcre)" --quit \
  && sbcl --eval "(ql:quickload :xembed)" --quit \
  && sbcl --eval "(ql:quickload :alexandria)" --quit \
  && sbcl --eval "(ql:quickload :clx-truetype)" --quit

prepare-truetype:
	make prepare-repo REPO_URL=https://github.com/dmb2/clx-freetype2 REPO_DIR=${QLLP}/clx-freetype2

prepare-repo:
	@  [[ -d ${REPO_DIR}/.git ]] \
  && cd ${REPO_DIR} \
  && git pull \
  && cd - \
  || git clone ${REPO_URL} ${REPO_DIR}

.PHONY: relink unlink link prepare-libs prepare-repo install-stumpwm install-contrib
