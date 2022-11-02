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
# texi-po/index.html and texi-ja/tramp.html publish to docs/ and docs/htmldocs
cd ${PROJ}/texi-po
make -f ${PROJ}/texi-po/publish-html.mak
exitcode=$?
if [ ${exitcode} -ne 0 ]; then
    notify-send -u critical tramp-docs-ja "publish-html.mak エラー"
    exit ${exitcode}
fi
cd ${PROJ}/texi-po
notify-send -u normal tramp-docs-ja "compile完了。"

