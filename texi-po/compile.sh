#!/bin/sh
PROJ=${HOME}/work/tramp-docs-ja
cd ${PROJ}/texi-po
make ja
exitcode=$?
if [ ${exitcode} -ne 0 ]; then
    notify-send -u critical tramp-docs-ja "texi-po/Makefile エラー"
    exit ${exitcode}
fi
cd ${PROJ}/texi-ja
make tramp html
exitcode=$?
if [ ${exitcode} -ne 0 ]; then
    notify-send -u critical tramp-docs-ja "texi-ja/Makefile エラー"
    exit ${exitcode}
fi
# gen file "dir" for info. and publish to docs/info/
cd ${PROJ}/info
make -f ${PROJ}/texi-po/publish-info.mak
exitcode=$?
if [ ${exitcode} -ne 0 ]; then
    notify-send -u critical tramp-docs-ja "publish-info.mak エラー"
    exit ${exitcode}
fi
# for github pages
# DIFF=diff ${PROJ}/texi-po/install-webdoc-only-html.sh ${PROJ}/texi-ja
# gawk -f ${PROJ}/texi-po/publish-index.awk TEMPLATE=${PROJ}/texi-po/index.html.template OUTPUT=${PROJ}/texi-ja/index.html < ${PROJ}/GIT-VERSION-FILE
# exitcode=$?
# if [ ${exitcode} -ne 0 ]; then
#     notify-send -u critical tramp-docs-ja "publish-index.awk エラー"
#     exit ${exitcode}
# fi
#
cd ${PROJ}/texi-po
notify-send -u normal git-docs-ja "compile完了。"

