#!/bin/bash
pushd ${HOME}/work/tramp-docs-ja/texi-po/
ls -1 *.po  | gawk -f ./po_count.awk
date
popd
