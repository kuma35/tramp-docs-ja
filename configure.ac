#  Autoconf script for Tramp                            -*- Autoconf -*-
# To rebuild the `configure' script from this, execute the command
#     autoconf
# in the directory containing this script.
#
#  Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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
AC_INIT([Tramp], [2.0.41], [tramp-devel@mail.freesoftware.fsf.org])
AC_MSG_NOTICE([$PACKAGE_STRING])

# The last version of Tramp that was merged into the Emacs CVS
# repository.  This will usually be the Tramp version minus one.
TRAMP_EMACS_MERGE_VERSION=2.0.40
AC_SUBST(TRAMP_EMACS_MERGE_VERSION)

# Check suitable make version
AC_CHECK_PROGS([MAKE], [gmake make], [no])
if test "${MAKE}" = no; then
   AC_MSG_ERROR([make not found])
fi

AC_MSG_CHECKING([for reasonable ${MAKE} version])
if test "$(${MAKE} -s -f test.makefile 123)" = 123; then
   AC_MSG_RESULT([ok])
else
   AC_MSG_RESULT([nok])
   AC_MSG_ERROR([${MAKE} is not up-to-date, you need at least GNU make 3.76])
fi

AC_PROG_MAKE_SET

# Find the Emacs program & flavor to be used
AC_EMACS_INFO

# Handle contrib files. "-" must be encoded as "_".
TRAMP_CONTRIB_FILES=

AC_CONTRIB_FILES([base64_encode_region],   [base64.el])
AC_CONTRIB_FILES([format_spec],            [format_spec.el])
AC_CONTRIB_FILES([md5],                    [md5.el],         [optional])
AC_CONTRIB_FILES([password_cache_add],     [password.el],    [optional])
AC_CONTRIB_FILES([uudecode_decode_region], [uudecode.el])
dnl run-at-time for XEmacs can be find in fsf-compat. Unfortunately,
dnl password.el doesn't honor it, and it requires run-at-time mandatory.
if test `echo "${EMACS}" | grep xemacs -`; then
   AC_CONTRIB_FILES([],                     [run_at_time.el], [optional])
else
   AC_CONTRIB_FILES([run_at_time],          [run_at_time.el], [optional])
fi

AC_SUBST(TRAMP_CONTRIB_FILES)

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

# Check existence of install-info.  Again, we produce a warning only,
# because this shouldn't prevent Tramp exploitation
AC_CHECK_PROG([INSTALL_INFO], [install-info], [install-info])
if test -z "${INSTALL_INFO}"; then
   AC_PATH_PROG([INSTALL_INFO], [install-info], [no],
		[$PATH:/usr/sbin:/sbin:/usr/local/bin])
fi
if test "${INSTALL_INFO}" = no; then
   AC_MSG_WARN([install-info not found])
fi

# Checks whether Tramp installation chapter is needed.
# Necessary for maintainers only
AC_EMACS_INSTALL

# Check japanese manual
AC_JA_MANUAL

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

# Define files to be cleaned up.  Applicable in all directories
TRAMP_CLEAN_FILES=".\\\\\#* \\\\\#* *~"
TRAMP_CLEAN_FILES="$TRAMP_CLEAN_FILES *.aux *.cp *.cps *.dvi *.elc *.fn"
TRAMP_CLEAN_FILES="$TRAMP_CLEAN_FILES *.fns *.html *.info *.ky *.log *.pg"
TRAMP_CLEAN_FILES="$TRAMP_CLEAN_FILES *.tmp *.toc *.tp *.vr *.vrs"
AC_SUBST([TRAMP_CLEAN_FILES])
# *.elc files must be removed always.  Otherwise, there could be problems
# if they are created with the wrong Emacs flavor.
rm -f lisp/*.elc contrib/*.elc

# Produce the Makefiles
TRAMP_CONFIG_FILES="Makefile contrib/Makefile"
TRAMP_CONFIG_FILES="$TRAMP_CONFIG_FILES lisp/Makefile lisp/trampver.el"
TRAMP_CONFIG_FILES="$TRAMP_CONFIG_FILES texi/Makefile texi/trampver.texi"
AC_SUBST(TRAMP_CONFIG_FILES)
AC_CONFIG_FILES([$TRAMP_CONFIG_FILES])

AC_OUTPUT
