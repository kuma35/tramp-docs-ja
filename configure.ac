#  Autoconf script for Tramp                            -*- Autoconf -*-
# To rebuild the `configure' script from this, execute the command
#     autoconf
# in the directory containing this script.
#
#  Copyright (C) 2003 Free Software Foundation, Inc.
#
#  This file is part of GNU Emacs.
#
#  GNU Emacs is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2, or (at your option)
#  any later version.
#
#  GNU Emacs is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with GNU Emacs; see the file COPYING.  If not, write to the
#  Free Software Foundation, Inc., 59 Temple Place - Suite 330,
#  Boston, MA 02111-1307, USA.

### Code:

# I've tested it with 2.52.  According to the Changelog of Autoconf,
# incompatible changes (with quotation) have appeared since 2.50.
AC_PREREQ([2.50])

# Initialize configure script.  This is the only point Tramp revision
# needs to be set.
AC_INIT([Tramp], [2.0.31], [tramp-devel@mail.freesoftware.fsf.org])
AC_MSG_NOTICE([$PACKAGE_STRING])
AC_PROG_MAKE_SET

# Find the Emacs program & flavor to be used
AC_EMACS_INFO

# Check suitable makeinfo version
AC_CHECK_PROG([MAKEINFO], [makeinfo], [makeinfo], [no])
if test "${MAKEINFO}" = no; then
   AC_MSG_ERROR([makeinfo not found])
fi

AC_MSG_CHECKING([for reasonable makeinfo version])
${MAKEINFO} texi/test.texi -o /dev/null 2>/dev/null >/dev/null
if test $? -eq 0; then
   AC_MSG_RESULT([ok])
else
   AC_MSG_RESULT([nok])
   AC_MSG_ERROR([makeinfo is not up-to-date, you need at least Texinfo 4.2])
fi

# Check suitable texi2dvi version.  Here we produce a warning only,
# because this shouldn't prevent Tramp exploitation
AC_CHECK_PROG([TEXI2DVI], [texi2dvi], [texi2dvi], [no])
if test "${TEXI2DVI}" = no; then
   AC_MSG_WARN([texi2dvi not found])
else
   AC_MSG_CHECKING([for reasonable texi2dvi version])
   ${TEXI2DVI} -b -q -c texi/test.texi 2>/dev/null >/dev/null
   if test $? -eq 0; then
      AC_MSG_RESULT([ok])
   else
      AC_MSG_RESULT([nok])
      AC_MSG_WARN([texi2dvi doesn't work, check \$TEXINPUTS])
      TEXI2DVI="no"
   fi
   test -e test.log && rm -f test.log
fi

# Check target installation directories
AC_SUBST(prefix)
AC_SUBST(prefix_default, $ac_default_prefix)
AC_SUBST(datadir)

AC_PATH_LISPDIR
AC_PATH_INFODIR

# Create local info directory
if test ! -d info; then
   AC_MSG_NOTICE([creating info/])
   AS_MKDIR_P([info])
fi

# Define trampinst.texi file, depending on --with-packaging option
AC_EMACS_INSTALL

# Define files to be cleaned up.  Applicable in all directories
TRAMP_CLEAN_FILES=".\\\\\#* \\\\\#* *~"
TRAMP_CLEAN_FILES="$TRAMP_CLEAN_FILES *.aux *.cp *.cps *.dvi *.elc *.fn"
TRAMP_CLEAN_FILES="$TRAMP_CLEAN_FILES *.fns *.html *.info *.ky *.log *.pg"
TRAMP_CLEAN_FILES="$TRAMP_CLEAN_FILES *.tmp *.toc *.tp *.vr *.vrs"
AC_SUBST([TRAMP_CLEAN_FILES])

# Produce the Makefiles
TRAMP_CONFIG_FILES="Makefile contrib/Makefile"
TRAMP_CONFIG_FILES="$TRAMP_CONFIG_FILES lisp/Makefile lisp/trampver.el"
TRAMP_CONFIG_FILES="$TRAMP_CONFIG_FILES texi/Makefile texi/trampver.texi"
AC_SUBST(TRAMP_CONFIG_FILES)
AC_CONFIG_FILES([$TRAMP_CONFIG_FILES])

AC_OUTPUT
