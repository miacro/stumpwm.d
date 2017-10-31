#!/bin/sh
ARGUMENTS="sbcl --noinform"
if [[ -f ~/.sbclrc ]] 
then
  ARGUMENTS="${ARGUMENTS} --load $(realpath ~/.sbclrc)"
fi
shift
exec ${ARGUMENTS} --script $(realpath $(dirname $0))/test.lisp $@

