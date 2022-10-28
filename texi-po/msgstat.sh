#!/bin/bash
pushd ${HOME}/work/tramp-docs-ja/texi-po/
ls -1 *.po | awk -f po_stat.awk $1
date
popd
