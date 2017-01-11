SOURCE_DIR=`pwd`
TARGET_DIR=${HOME}/.stumpwm.d
MAKE=make --no-print-directory

reinstall:
	@  ${MAKE} uninstall \
	&& ${MAKE} install

install:
	@  [[ ! -L ${TARGET_DIR} ]] && [[ ! -f ${TARGET_DIR} ]] \
	&& ln -s ${SOURCE_DIR} ${TARGET_DIR} \
	||  exit 0

uninstall:
	@  [[ -L ${TARGET_DIR} ]] \
	&& rm ${TARGET_DIR} \
	|| exit 0

.PHONY: reinstall uninstall install 
