;;; rcp.el --- remote file editing using rsh/rcp or work-alike programs

;; Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Kai.Grossjohann@CS.Uni-Dortmund.DE 
;; Keywords: comm, processes
;; Version: $Id: tramp.el,v 1.269 2000/04/15 23:42:47 grossjoh Exp $

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides remote file editing, similar to ange-ftp.
;; The difference is that ange-ftp uses FTP to transfer files between
;; the local and the remote host, whereas rcp.el uses a combination
;; of rsh and rcp or other work-alike programs, such as ssh/scp.
;;
;; Installation is simple -- it is sufficient to load this file.  EFS
;; users should do (require 'efs) before loading this file, though.
;; This is such that the regexes for rcp files come before the
;; regexes for EFS files in `file-name-handler-alist'.
;;
;; Usage is also simple: it's just like ange-ftp, but uses a different
;; syntax for the remote file names.  The syntax used is as follows:
;;
;; /r@METHOD:USER@HOST:FILENAME
;;
;; This logs you in as USER to the remote HOST using METHOD,
;; retrieving FILENAME.  The "USER@" part can be omitted, in this case
;; the current local user name is used.  The "@METHOD" part can be
;; omitted, in this case the default method specified in
;; `rcp-default-method' is used (default value is "rcp").
;;
;; There are a number of different access methods, listed in
;; `rcp-methods'.  The different methods can be divided into two
;; groups, I call them `out-of-band' and `inline' methods.  One
;; example for an out-of-band method is "rcp" which uses "rsh" to
;; connect to the remote machine and to get a list of files and the
;; like, but the actual file transfer is done via the program "rcp".
;; One example for an inline method is "rm" which uses "rsh" to
;; connect to the remote machine and to get a list of files and the
;; like, and the actual file transfer is done by issuing the command
;; "mimencode -b < FILENAME" to the remote side, and the encoded file
;; contents are then piped into the command "mimencode -b -u >
;; TMPFILE" for decoding.
;;
;; The out-of-band methods require that you can log in to the remote
;; system without having to enter a password.  This is because the
;; standard program "rcp" does not query for a password but just fails
;; if entering a password is necessary.
;;
;; After starting `rsh' or `telnet', this package looks for a shell
;; prompt from the remote side.  Therefore, it is necessary for you to
;; set the variable `shell-prompt-pattern' correctly such that the
;; remote shell prompts are recognized.  (Please tell me, Kai, about
;; it if you think that this is a problem.)
;;
;; This package has received some testing, but there is little error
;; recovery code.  That is, if something unexpected happens, this
;; package will bug out with a potentially very cryptic error message.
;; Please help me improve this package by telling me about these
;; unusual situations.
;;
;; Known problems:
;;   - There is no error checking which prevents you to use an
;;     out-of-band method if you have to enter a password to connect
;;     to the remote side.
;;   - BSD ls doesn't grok `-n' option for printing numeric user and
;;     group ids.  Use `gnuls' instead.
;;   - Using EFS and rcp together in XEmacs may have some problems.
;;     Please report any issues as this is actively developed.
;;   - This code requires the macro `with-timeout' which does not
;;     seem to be part of XEmacs 20.  Can you upgrade to XEmacs 21?
;;
;; Also see the todo list at the bottom of this file.
;;
;; The current version of rcp.el can be retrieved from the following
;; URL:  ftp://ls6-ftp.cs.uni-dortmund.de/pub/src/emacs/rcp.tar.gz
;; For your convenience, the *.el file is available separately from
;; the same directory.  Additionally, there's a second tarball which
;; contains the RCS files.
;;
;; There's a mailing list for this, as well.  Its name is:
;;                emacs-rcp@ls6.cs.uni-dortmund.de
;; Send a mail with `help' in the subject (!) to the administration
;; address for instructions on joining the list.  The administration
;; address is:
;;            emacs-rcp-request@ls6.cs.uni-dortmund.de
;; You may also mail me, Kai, directly.

;;; Code:

(defconst rcp-version "$Id: tramp.el,v 1.269 2000/04/15 23:42:47 grossjoh Exp $"
  "This version of rcp.")
(defconst rcp-bug-report-address "emacs-rcp@ls6.cs.uni-dortmund.de"
  "Email address to send bug reports to.")

(require 'timer)

;; CCC: The following require should be removed once the integration
;; with VC is clear.  Talking to Andre Spiegel about this.
(require 'vc)                           ;for doing remote vc

(eval-when-compile
  (require 'cl)
  (require 'custom)
  ;; Emacs 19.34 compatibility hack -- is this needed?
  (or (>= emacs-major-version 20)
      (load "cl-seq")))

(unless (boundp 'custom-print-functions)
  (defvar custom-print-functions nil))	; not autoloaded before Emacs 20.4

;;; User Customizable Internal Variables:

(defgroup rcp nil
  "Edit remote files with a combination of rsh and rcp or similar programs."
  :group 'files)

(defcustom rcp-verbose 3
  "*Verbosity level for rcp.el.  0 means be silent, 10 is most verbose."
  :group 'rcp
  :type 'integer)

(defcustom rcp-debug-buffer nil
  "*Whether to send all commands and responses to a debug buffer."
  :group 'rcp
  :type 'boolean)

(defcustom rcp-auto-save-directory nil
  "*Put auto-save files in this directory, if set.
The idea is to use a local directory so that auto-saving is faster."
  :group 'rcp
  :type '(choice (const nil)
                 string))

(defcustom rcp-sh-program "/bin/sh"
  "*Use this program for shell commands on the local host.
This MUST be a Bourne-like shell.  This shell is used to execute
the encoding and decoding command on the local host, so if you
want to use `~' in those commands, you should choose a shell here
which groks tilde expansion.  `/bin/sh' normally does not
understand tilde expansion.

Note that this variable is not used for remote commands.  There are
mechanisms in rcp.el which automatically determine the right shell to
use for the remote host."
  :group 'rcp
  :type '(file :must-match t))

;; CCC I have changed all occurrences of comint-quote-filename with
;; rcp-shell-quote-argument, except in rcp-handle-expand-many-files.
;; There, comint-quote-filename was removed altogether.  If it turns
;; out to be necessary there, something will need to be done.
;;-(defcustom rcp-file-name-quote-list
;;-  '(?] ?[ ?\| ?& ?< ?> ?\( ?\) ?\; ?\  ?\* ?\? ?\! ?\" ?\' ?\` ?# ?\@ ?\+ )
;;-  "*Protect these characters from the remote shell.
;;-Any character in this list is quoted (preceded with a backslash)
;;-because it means something special to the shell.  This takes effect
;;-when sending file and directory names to the remote shell.
;;-
;;-See `comint-file-name-quote-list' for details."
;;-  :group 'rcp
;;-  :type '(repeat character))

(defcustom rcp-methods
  '( ("rcp"   (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "rsh")
              (rcp-rcp-program          "rcp")
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    "-p")
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     nil)
              (rcp-decoding-command     nil)
              (rcp-encoding-function    nil)
              (rcp-decoding-function    nil)
              (rcp-telnet-program       nil))
     ("scp"   (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh")
              (rcp-rcp-program          "scp")
              (rcp-rsh-args             ("-e" "none"))
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    "-p")
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     nil)
              (rcp-decoding-command     nil)
              (rcp-encoding-function    nil)
              (rcp-decoding-function    nil)
              (rcp-telnet-program       nil))
     ("scp1"  (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh1")
              (rcp-rcp-program          "scp1")
              (rcp-rsh-args             ("-e" "none"))
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    "-p")
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     nil)
              (rcp-decoding-command     nil)
              (rcp-encoding-function    nil)
              (rcp-decoding-function    nil)
              (rcp-telnet-program       nil))
     ("scp2"  (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh2")
              (rcp-rcp-program          "scp2")
              (rcp-rsh-args             ("-e" "none"))
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    "-p")
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     nil)
              (rcp-decoding-command     nil)
              (rcp-encoding-function    nil)
              (rcp-decoding-function    nil)
              (rcp-telnet-program       nil))
     ("rsync" (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh")
              (rcp-rcp-program          "rsync")
              (rcp-rsh-args             ("-e" "none"))
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    "-t")
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     nil)
              (rcp-decoding-command     nil)
              (rcp-encoding-function    nil)
              (rcp-decoding-function    nil)
              (rcp-telnet-program       nil))
     ("ru"    (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "rsh")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     "uuencode xxx")
              (rcp-decoding-command
               "( uudecode -o - 2>/dev/null || uudecode -p 2>/dev/null )")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil))
     ("su"    (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     "uuencode xxx")
              (rcp-decoding-command
               "( uudecode -o - 2>/dev/null || uudecode -p 2>/dev/null )")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil))
     ("su1"   (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh1")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     "uuencode xxx")
              (rcp-decoding-command
               "( uudecode -o - 2>/dev/null || uudecode -p 2>/dev/null )")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil))
     ("su2"   (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh2")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     "uuencode xxx")
              (rcp-decoding-command
               "( uudecode -o - 2>/dev/null || uudecode -p 2>/dev/null )")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil))
     ("rm"    (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "rsh")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil))
     ("sm"    (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil))
     ("sm1"   (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh1")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil))
     ("sm2"   (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh2")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil))
     ("tm"    (rcp-connection-function  rcp-open-connection-telnet)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       "telnet"))
     ("tu"    (rcp-connection-function  rcp-open-connection-telnet)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     "uuencode xxx")
              (rcp-decoding-command
               "( uudecode -o - 2>/dev/null || uudecode -p 2>/dev/null )")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       "telnet"))
     ("sum"   (rcp-connection-function  rcp-open-connection-su)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           "su")
              (rcp-su-args              ("-"))
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil))
     ("suu"   (rcp-connection-function  rcp-open-connection-su)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           "su")
              (rcp-su-args              ("-"))
              (rcp-encoding-command     "uuencode xxx")
              (rcp-decoding-command
               "( uudecode -o - 2>/dev/null || uudecode -p 2>/dev/null )")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil))
     ("multi" (rcp-connection-function  rcp-open-connection-multi)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil))
     ("multiu" (rcp-connection-function  rcp-open-connection-multi)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-su-program           nil)
              (rcp-su-args              nil)
              (rcp-encoding-command     "uuencode xxx")
              (rcp-decoding-command
               "( uudecode -o - 2>/dev/null || uudecode -p 2>/dev/null )")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil))
     )
  "*Alist of methods for remote files.
This is a list of entries of the form (NAME PARAM1 PARAM2 ...).
Each NAME stands for a remote access method.  Each PARAM is a
pair of the form (KEY VALUE).  The following KEYs are defined:
  * `rcp-connection-function'
    This specifies the function to use to connect to the remote host.
    Currently, `rcp-open-connection-rsh', `rcp-open-connection-telnet'
    and `rcp-open-connection-su' are defined.  See the documentation
    of these functions for more details.
  * `rcp-rsh-program'
    This specifies the name of the program to use for rsh; this might be
    the full path to rsh or the name of a workalike program.
  * `rcp-rsh-args'
    This specifies the list of arguments to pass to the above mentioned
    program.  Please note that this is a list of arguments, that is,
    normally you don't want to put \"-a -b\" here.  Instead, you want
    two list elements, one for \"-a\" and one for \"-b\".
  * `rcp-rcp-program'
    This specifies the name of the program to use for rcp; this might be
    the full path to rcp or the name of a workalike program.
  * `rcp-rcp-args'
    This specifies the list of parameters to pass to the above mentioned
    program, the hints for `rcp-rsh-args' also apply here.
  * `rcp-rcp-keep-date-arg'
    This specifies the parameter to use for `rcp' when the timestamp
    of the original file should be kept.  For `rcp', use `-p', for
    `rsync', use `-t'.
  * `rcp-su-program'
    This specifies the name of the program to use for `su'.
  * `rcp-su-args'
    This specifies the list of arguments to pass to `su'.
    The user name is always passed as the last argument, in addition
    to the arguments specified here.
  * `rcp-encoding-command'
    This specifies a command to use to encode the file contents for
    transfer.  The command should read the raw file contents from
    standard input and write the encoded file contents to standard
    output.  In this string, the percent escape \"%f\" should be used
    to indicate the file to convert.  Use \"%%\" if you need a literal
    percent character in your command.
  * `rcp-decoding-command'
    This specifies a command to use to decode file contents encoded
    with `rcp-encoding-command'.  The command should read from standard
    input and write to standard output.
  * `rcp-encoding-function'
    This specifies a function to be called to encode the file contents
    on the local side.  This function should accept two arguments
    START and END, the beginning and end of the region to encode.  The
    region should be replaced with the encoded contents.
  * `rcp-decoding-function'
    Same for decoding on the local side.
  * `rcp-telnet-program'
    Specifies the telnet program to use when using
    `rcp-open-connection-telnet' to log in.

What does all this mean?  Well, you should specify `rcp-rsh-program',
`rcp-telnet-program' or `rcp-su-program' for all methods; this program
is used to log in to the remote site.  Then, there are two ways to
actually transfer the files between the local and the remote side.
One way is using an additional rcp-like program.  If you want to do
this, set `rcp-rcp-program' in the method.

Another possibility for file transfer is inline transfer, i.e. the
file is passed through the same buffer used by `rcp-rsh-program'.  In
this case, the file contents need to be protected since the
`rcp-rsh-program' might use escape codes or the connection might not
be eight-bit clean.  Therefore, file contents are encoded for transit.

Two possibilities for encoding are uuencode/uudecode and mimencode.
For uuencode/uudecode you want to set `rcp-encoding-command' to
something like \"uuencode\" and `rcp-decoding-command' to \"uudecode
-p\".  For mimencode you want to set `rcp-encoding-command' to
something like \"mimencode -b\" and `rcp-decoding-command' to
\"mimencode -b -u\".

When using inline transfer, you can use a program or a Lisp function
on the local side to encode or decode the file contents.  Set the
`rcp-encoding-function' and `rcp-decoding-function' parameters to nil
in order to use the commands or to the function to use.  It is
possible to specify one function and the other parameter as nil.

Notes:

When using `rcp-open-connection-su' the phrase `open connection to a
remote host' sounds strange, but it is used nevertheless, for
consistency.  No connection is opened to a remote host, but `su' is
started on the local host.  You are not allowed to specify a remote
host other than `localhost' or the name of the local host.

Using a uuencode/uudecode inline method is discouraged, please use one
of the base64 methods instead since base64 encoding is much more
reliable and the commands are more standardized between the different
Unix versions.  But if you can't use base64 for some reason, please
note that the default uudecode command does not work well for some
Unices, in particular AIX and Irix.  For AIX, you might want to use
the following command for uudecode:

    sed '/^begin/d;/^[` ]$/d;/^end/d' | iconv -f uucode -t ISO8859-1

For Irix, no solution is known yet."
  :group 'rcp
  :type '(repeat
          (cons string
                (set (list (const rcp-connection-function) function)
                     (list (const rcp-rsh-program) string)
                     (list (const rcp-rcp-program) string)
                     (list (const rcp-rsh-args) (repeat string))
                     (list (const rcp-rcp-args) (repeat string))
                     (list (const rcp-rcp-keep-date-arg) string)
                     (list (const rcp-su-program) string)
                     (list (const rcp-su-args) (repeat string))
                     (list (const rcp-encoding-command) string)
                     (list (const rcp-decoding-command) string)
                     (list (const rcp-encoding-function) function)
                     (list (const rcp-decoding-function) function)
                     (list (const rcp-telnet-program) string)))))

(defcustom rcp-multi-methods '("multi" "multiu")
  "*List of multi-hop methods.
A multi-hop method is a method where you can specify multiple \(user
name, host name\) pairs; opening a connection to the remote host is
done by `hopping' from one host to the other as specified in this
list.

Not implemented yet."
  :group 'rcp
  :type '(repeat string))

(defcustom rcp-multi-connection-function-alist
  '(("telnet" rcp-multi-connect-telnet "telnet")
    ("rsh"    rcp-multi-connect-rlogin "rsh")
    ("ssh"    rcp-multi-connect-rlogin "ssh")
    ("su"     rcp-multi-connect-su     "su"))
  "*List of connection functions for multi-hop methods.
Each list item is a list of three items (METHOD FUNCTION PROGRAM),
where METHOD is the name as used in the file name, FUNCTION is the
function to be executed, and PROGRAM is the program used for
connecting."
  :group 'rcp
  :type '(repeat (list string function string)))

(defcustom rcp-default-method "rcp"
  "*Default method to use for transferring files.
See `rcp-methods' for possibilities."
  :group 'rcp
  :type 'string)

(defcustom rcp-rsh-end-of-line "\n"
  "*String used for end of line in rsh connections.
I don't think this ever needs to be changed, so please tell me about it
if you need to change this."
  :group 'rcp
  :type 'string)

(defcustom rcp-remote-path
  '("/bin" "/usr/bin" "/usr/sbin" "/usr/local/bin" "/usr/ccs/bin")
  "*List of directories to search for executables on remote host.
Please notify me about other semi-standard directories to include here."
  :group 'rcp
  :type '(repeat string))

(defcustom rcp-password-prompt-regexp
  "^.*\\([pP]assword\\|passphrase.*\\): *$"
  "*Regexp matching password-like prompts.  Not used for telnet.
The regexp should match the whole line.
\(The prompt for telnet is hard-wired.)"
  :group 'rcp
  :type 'regexp)

(defcustom rcp-wrong-passwd-regexp
  (concat "^.*\\(Permission denied.\\|Login [Ii]ncorrect\\|"
          "Received signal [0-9]+\\|Connection \\(refused\\|closed\\)\\).*$")
  "*Regexp matching a `login failed' message.
The regexp should match the whole line."
  :group 'rcp
  :type 'regexp)

(defcustom rcp-temp-name-prefix "rcp."
  "*Prefix to use for temporary files.
If this is a relative file name (such as \"rcp.\"), it is considered
relative to the directory name returned by the function
`rcp-temporary-file-directory' (which see).  It may also be an
absolute file name; don't forget to include a prefix for the filename
part, though."
  :group 'rcp
  :type 'string)

;; File name format.

(defcustom rcp-file-name-structure
  (list "\\`/r\\(@\\([a-z0-9]+\\)\\)?:\\(\\([a-z0-9_#]+\\)@\\)?\\([a-z0-9.-]+\\):\\(.*\\)\\'"
        2 4 5 6)
  "*List of five elements (REGEXP METHOD USER HOST FILE), detailing \
the rcp file name structure.

The first element REGEXP is a regular expression matching an rcp file
name.  The regex should contain parentheses around the method name,
the user name, the host name, and the file name parts.

The second element METHOD is a number, saying which pair of
parentheses matches the method name.  The third element USER is
similar, but for the user name.  The fourth element HOST is similar,
but for the host name.  The fifth element FILE is for the file name.
These numbers are passed directly to `match-string', which see.  That
means the opening parentheses are counted to identify the pair.

See also `rcp-file-name-regexp' and `rcp-make-rcp-file-format'."
  :group 'rcp
  :type '(list (regexp :tag "File name regexp")
               (integer :tag "Paren pair for method name")
               (integer :tag "Paren pair for user name  ")
               (integer :tag "Paren pair for host name  ")
               (integer :tag "Paren pair for file name  ")))

(defcustom rcp-file-name-regexp "\\`/r[@:]"
  "*Regular expression matching file names handled by rcp.
This regexp should match rcp file names but no other file names.
\(When rcp.el is loaded, this regular expression is prepended to
`file-name-handler-alist', and that is searched sequentially.  Thus,
if the rcp entry appears rather early in the `file-name-handler-alist'
and is a bit too general, then some files might be considered rcp
files which are not really rcp files.

Please note that the entry in `file-name-handler-alist' is made when
this file (rcp.el) is loaded.  This means that this variable must be set
before loading rcp.el.  Alternatively, `file-name-handler-alist' can be
updated after changing this variable.

Also see `rcp-file-name-structure' and `rcp-make-rcp-file-format'."
  :group 'rcp
  :type 'regexp)

(defcustom rcp-make-rcp-file-format "/r@%m:%u@%h:%p"
  "*Format string saying how to construct rcp file name.
`%m' is replaced by the method name.
`%u' is replaced by the user name.
`%h' is replaced by the host name.
`%p' is replaced by the file name.
`%%' is replaced by %.

Also see `rcp-file-name-structure' and `rcp-file-name-regexp'."
  :group 'rcp
  :type 'string)

(defcustom rcp-multi-file-name-structure
  (list (concat
         ;; prefix
         "\\`/r\\(@\\([a-z0-9]+\\)\\)?:"
         ;; regexp specifying a hop
         "\\(\\(%s\\)+\\)"
         ;; path name
         "\\(.*\\)\\'")
        2                               ;number of pair to match method
        3                               ;number of pair to match hops
        -1)                             ;number of pair to match path

  "*Describes the file name structure of `multi' files.
Multi files allow you to contact a remote host in several hops.
`%s' is replaced with the regexp from `rcp-multi-file-name-hop-structure'.

CCC: This documentation needs to be finished.  This will be difficult."
  :group 'rcp
  :type '(list (regexp :tag "File name regexp")
               (integer :tag "Paren pair for method name")
               (integer :tag "Paren pair for hops")
               (integer :tag "Paren pair to match path")))

(defcustom rcp-multi-file-name-hop-structure
  (list "\\([a-z]+\\)#\\([a-z0-9_]+\\)@\\([a-z0-9.-]+\\):"
        1 2 3)
  "*Describes the structure of a hop in multi files.
List of four elements (REGEXP METHOD USER HOST).  First element REGEXP
is used to match against the hop.  Pair number METHOD matches the
method of one hop, pair number USER matches the user of one hop, pair
number HOST matches the host of one hop."
  :group 'rcp
  :type '(list (regexp :tag "Hop regexp")
               (integer :tag "Paren pair for method name")
               (integer :tag "Paren pair for user name")
               (integer :tag "Paren pair for host name")))

(defcustom rcp-make-multi-rcp-file-format
  (list "/r@%m:" "%m#%u@%h:" "%p")
  "*Describes how to construct a `multi' file name.
This is a list of three elements PREFIX, HOP and PATH.

The first element PREFIX says how to construct the prefix, the second
element HOP specifies what each hop looks like, and the final element
PATH says how to construct the path name.

In PREFIX, `%%' means `%' and `%m' means the method name.

In HOP, `%%' means `%' and `%m', `%u', `%h' mean the hop method, hop
user and hop host, respectively.

In PATH, `%%' means `%' and `%p' means the path name."
  :group 'rcp
  :type '(list string string string))

;;; Internal Variables:

(defvar rcp-end-of-output "/////"
  "String used to recognize end of output.")

(defvar rcp-connection-function nil
  "This internal variable holds a parameter for `rcp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `rcp-methods' (which see).")

(defvar rcp-rsh-program nil
  "This internal variable holds a parameter for `rcp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `rcp-methods' (which see).")

(defvar rcp-rsh-args nil
  "This internal variable holds a parameter for `rcp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `rcp-methods' (which see).")

(defvar rcp-rcp-program nil
  "This internal variable holds a parameter for `rcp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `rcp-methods' (which see).")

(defvar rcp-rcp-args nil
  "This internal variable holds a parameter for `rcp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `rcp-methods' (which see).")

(defvar rcp-rcp-keep-date-arg nil
  "This internal variable holds a parameter for `rcp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `rcp-methods' (which see).")

(defvar rcp-encoding-command nil
  "This internal variable holds a parameter for `rcp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `rcp-methods' (which see).")

(defvar rcp-decoding-command nil
  "This internal variable holds a parameter for `rcp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `rcp-methods' (which see).")

(defvar rcp-encoding-function nil
  "This internal variable holds a parameter for `rcp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `rcp-methods' (which see).")

(defvar rcp-decoding-function nil
  "This internal variable holds a parameter for `rcp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `rcp-methods' (which see).")

(defvar rcp-telnet-program nil
  "This internal variable holds a parameter for `rcp-methods'.
In the connection buffer, this variable has the value of the like-named
method parameter, as specified in `rcp-methods' (which see).")

;; CCC `local in each buffer'?
(defvar rcp-ls-command nil
  "This command is used to get a long listing with numeric user and group ids.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar rcp-current-multi-method nil
  "Name of `multi' connection method for this *rcp* buffer, or nil if not multi.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar rcp-current-method nil
  "Connection method for this *rcp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar rcp-current-user nil
  "Remote login name for this *rcp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar rcp-current-host nil
  "Remote host for this *rcp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

;; New handlers should be added here.  The following operations can be
;; handled using the normal primitives: file-name-as-directory,
;; file-name-directory, file-name-nondirectory,
;; file-name-sans-versions, get-file-buffer.
(defconst rcp-file-name-handler-alist
  '((file-name-directory . rcp-handle-file-name-directory)
    (file-name-nondirectory . rcp-handle-file-name-nondirectory)
    (file-exists-p . rcp-handle-file-exists-p)
    (file-directory-p . rcp-handle-file-directory-p)
    (file-executable-p . rcp-handle-file-executable-p)
    (file-accessible-directory-p . rcp-handle-file-accessible-directory-p)
    (file-readable-p . rcp-handle-file-readable-p)
    (file-regular-p . rcp-handle-file-regular-p)
    (file-symlink-p . rcp-handle-file-symlink-p)
    (file-writable-p . rcp-handle-file-writable-p)
    (file-attributes . rcp-handle-file-attributes)
    (file-modes . rcp-handle-file-modes)
    (file-directory-files . rcp-handle-file-directory-files)
    (directory-files . rcp-handle-directory-files)
    (file-name-all-completions . rcp-handle-file-name-all-completions)
    (file-name-completion . rcp-handle-file-name-completion)
    (add-name-to-file . rcp-handle-add-name-to-file)
    (copy-file . rcp-handle-copy-file)
    (rename-file . rcp-handle-rename-file)
    (make-directory . rcp-handle-make-directory)
    (delete-directory . rcp-handle-delete-directory)
    (delete-file . rcp-handle-delete-file)
    (directory-file-name . rcp-handle-directory-file-name)
    (shell-command . rcp-handle-shell-command)
    (insert-directory . rcp-handle-insert-directory)
    (expand-file-name . rcp-handle-expand-file-name)
    (file-local-copy . rcp-handle-file-local-copy)
    (insert-file-contents . rcp-handle-insert-file-contents)
    (write-region . rcp-handle-write-region)
    (unhandled-file-name-directory . rcp-handle-unhandled-file-name-directory)
    (dired-call-process . rcp-handle-dired-call-process)
    (dired-recursive-delete-directory
     . rcp-handle-dired-recursive-delete-directory))
        "Alist of handler functions.
Operations not mentioned here will be handled by the normal Emacs functions.")

;;; For better error reporting.

(defun rcp-version (arg)
  "Print version number of rcp.el in minibuffer or current buffer."
  (interactive "P")
  (if arg (insert rcp-version) (message rcp-version)))

;;; Internal functions which must come first.

(defsubst rcp-message (level fmt-string &rest args)
  "Emit a message depending on verbosity level.
First arg LEVEL says to be quiet if `rcp-verbose' is less than LEVEL.  The
message is emitted only if `rcp-verbose' is greater than or equal to LEVEL.
Calls function `message' with FMT-STRING as control string and the remaining
ARGS to actually emit the message (if applicable).

This function expects to be called from the rcp buffer only!"
  (when (<= level rcp-verbose)
    (apply #'message fmt-string args)
    (when rcp-debug-buffer
      (save-excursion
        (set-buffer
         (rcp-get-debug-buffer rcp-current-multi-method rcp-current-method
                               rcp-current-user rcp-current-host))
        (goto-char (point-max))
        (insert "# " (apply #'format fmt-string args) "\n")))))

(defun rcp-message-for-buffer
  (multi-method method user host level fmt-string &rest args)
  "Like `rcp-message' but temporarily switches to the rcp buffer.
First three args METHOD, USER, and HOST identify the rcp buffer to use,
remaining args passed to `rcp-message'."
  (save-excursion
    (set-buffer (rcp-get-buffer multi-method method user host))
    (apply 'rcp-message level fmt-string args)))

;;; File Name Handler Functions:

;; Path manipulation functions that grok RCP paths...
(defun rcp-handle-file-name-directory (file)
  "Like `file-name-directory' but aware of RCP files."
  ;; everything except the last filename thing is the directory
  (let* ((v	 (rcp-dissect-file-name file))
         (multi-method (rcp-file-name-multi-method v))
	 (method (rcp-file-name-method v))
	 (user   (rcp-file-name-user v))
	 (host   (rcp-file-name-host v))
	 (path   (rcp-file-name-path v)))
    ;; run the command on the path portion only
    ;; REVISIT: This should take into account the remote machine type, no?
    ;;  --daniel <daniel@danann.net>
    (rcp-make-rcp-file-name multi-method method user host
			    ;; This should not recurse...
			    (file-name-directory path))))

(defun rcp-handle-file-name-nondirectory (file)
  "Like `file-name-nondirectory' but aware of RCP files."
  (let ((v (rcp-dissect-file-name file)))
    (file-name-nondirectory (rcp-file-name-path v))))
  

;; Basic functions.

(defun rcp-handle-file-exists-p (filename)
  "Like `file-exists-p' for rcp files."
  (let ((v (rcp-dissect-file-name (rcp-handle-expand-file-name filename)))
        multi-method method user host path)
    (setq multi-method (rcp-file-name-multi-method v))
    (setq method (rcp-file-name-method v))
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (rcp-send-command
       multi-method method user host
       (format "%s -d %s ; echo $?"
               (rcp-get-ls-command multi-method method user host)
               (rcp-shell-quote-argument path)))
      (rcp-wait-for-output)
      (goto-char (point-max))
      (forward-line -1)
      (zerop (read (current-buffer))))))

;; CCC: This should check for an error condition and signal failure
;;      when something goes wrong.
;; Daniel Pittman <daniel@danann.net>
(defun rcp-handle-file-attributes (filename &optional nonnumeric)
  "Like `file-attributes' for rcp files.
Optional argument NONNUMERIC means return user and group name
rather than as numbers."
  (let ((v (rcp-dissect-file-name (rcp-handle-expand-file-name filename)))
        multi-method method user host path
        symlinkp dirp
        res-inode res-filemodes res-numlinks
        res-uid res-gid res-size res-symlink-target)
    (if (not (rcp-handle-file-exists-p filename))
        nil                             ; file cannot be opened
      ;; file exists, find out stuff
      (save-match-data
        (save-excursion
          (setq multi-method (rcp-file-name-multi-method v))
          (setq method (rcp-file-name-method v))
          (setq user (rcp-file-name-user v))
          (setq host (rcp-file-name-host v))
          (setq path (rcp-file-name-path v))
          (rcp-send-command
           multi-method method user host
           (format "%s %s %s"
                   (rcp-get-ls-command multi-method method user host)
                   (if nonnumeric "-iLld" "-iLldn")
                   (rcp-shell-quote-argument path)))
          (rcp-wait-for-output)
          ;; parse `ls -l' output ...
          ;; ... inode
          (setq res-inode
                (condition-case err
                    (read (current-buffer))
                  (invalid-read-syntax
                   (when (and (equal (cadr err)
                                     "Integer constant overflow in reader")
                              (string-match
                               "^[0-9]+\\([0-9][0-9][0-9][0-9][0-9]\\)\\'"
                               (caddr err)))
                     (let* ((big (read (substring (caddr err) 0
                                                  (match-beginning 1))))
                            (small (read (match-string 1 (caddr err))))
                            (twiddle (/ small 65536)))
                       (cons (+ big twiddle)
                             (- small (* twiddle 65536))))))))
          ;; ... file mode flags
          (setq res-filemodes (symbol-name (read (current-buffer))))
          ;; ... number links
          (setq res-numlinks (read (current-buffer)))
          ;; ... uid and gid
          (setq res-uid (read (current-buffer)))
          (setq res-gid (read (current-buffer)))
          (unless nonnumeric
            (unless (numberp res-uid) (setq res-uid -1))
            (unless (numberp res-gid) (setq res-gid -1)))
          ;; ... size
          (setq res-size (read (current-buffer)))
          ;; From the file modes, figure out other stuff.
          (setq symlinkp (eq ?l (aref res-filemodes 0)))
          (setq dirp (eq ?d (aref res-filemodes 0)))
          ;; if symlink, find out file name pointed to
          (when symlinkp
            (search-forward "-> ")
            (setq res-symlink-target
                  (buffer-substring (point)
                                    (progn (end-of-line) (point)))))))
      ;; return data gathered
      (list
       ;; 0. t for directory, string (name linked to) for symbolic
       ;; link, or nil.
       (or dirp res-symlink-target nil)
       ;; 1. Number of links to file.
       res-numlinks
       ;; 2. File uid.
       res-uid
       ;; 3. File gid.
       res-gid
       ;; 4. Last access time, as a list of two integers. First
       ;; integer has high-order 16 bits of time, second has low 16
       ;; bits.
       ;; 5. Last modification time, likewise.
       ;; 6. Last status change time, likewise.
       '(0 0) '(0 0) '(0 0)             ;CCC how to find out?
       ;; 7. Size in bytes (-1, if number is out of range).
       res-size
       ;; 8. File modes, as a string of ten letters or dashes as in ls -l.
       res-filemodes
       ;; 9. t iff file's gid would change if file were deleted and
       ;; recreated.
       nil                              ;hm?
       ;; 10. inode number.
       res-inode
       ;; 11. Device number.
       -1                               ;hm?
       ))))

;; Simple functions using the `test' command.

(defun rcp-handle-file-executable-p (filename)
  "Like `file-executable-p' for rcp files."
  (zerop (rcp-run-test "-x" filename)))

(defun rcp-handle-file-readable-p (filename)
  "Like `file-readable-p' for rcp files."
  (zerop (rcp-run-test "-r" filename)))

(defun rcp-handle-file-accessible-directory-p (filename)
  "Like `file-accessible-directory-p' for rcp files."
  (and (zerop (rcp-run-test "-d" filename))
       (zerop (rcp-run-test "-r" filename))
       (zerop (rcp-run-test "-x" filename))))

;; Functions implemented using the basic functions above.

(defun rcp-handle-file-modes (filename)
  "Like `file-modes' for rcp files."
  (when (file-exists-p filename)
    (rcp-mode-string-to-int
     (nth 8 (rcp-handle-file-attributes filename)))))

(defun rcp-handle-file-directory-p (filename)
  "Like `file-directory-p' for rcp files."
  (eq t (car (rcp-handle-file-attributes filename))))

(defun rcp-handle-file-regular-p (filename)
  "Like `file-regular-p' for rcp files."
  (and (rcp-handle-file-exists-p filename)
       (eq ?- (aref (nth 8 (rcp-handle-file-attributes filename)) 0))))

(defun rcp-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for rcp files."
  (stringp (car (rcp-handle-file-attributes filename))))

(defun rcp-handle-file-writable-p (filename)
  "Like `file-writable-p' for rcp files."
  (if (rcp-handle-file-exists-p filename)
      ;; Existing files must be writable.
      (zerop (rcp-run-test "-w" filename))
    ;; If file doesn't exist, check if directory is writable.
    (and (zerop (rcp-run-test "-d" (rcp-handle-file-name-directory filename)))
         (zerop (rcp-run-test "-w" (rcp-handle-file-name-directory filename))))))
       

;; Other file name ops.

(defun rcp-handle-file-truename (filename &optional counter prev-dirs)
  "Like `file-truename' for rcp files."
  filename)                             ;CCC what to do?

;; Matthias KÅˆppe <mkoeppe@mail.math.uni-magdeburg.de>
(defun rcp-handle-directory-file-name (directory)
  "Like `directory-file-name' for rcp files."
  (if (and (eq (aref directory (- (length directory) 1)) ?/)
	   (not (eq (aref directory (- (length directory) 2)) ?:)))
      (substring directory 0 (- (length directory) 1))
    directory))


;; Directory listings.

(defun rcp-handle-directory-files (directory &optional full match nosort)
  "Like `directory-files' for rcp files."
  (let ((v (rcp-dissect-file-name (rcp-handle-expand-file-name directory)))
        multi-method method user host path result x)
    (setq multi-method (rcp-file-name-multi-method v))
    (setq method (rcp-file-name-method v))
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (save-match-data
        (rcp-send-command multi-method method user host
                          (concat "cd " (rcp-shell-quote-argument path)
                                  " ; echo $?"))
        (rcp-barf-unless-okay
         "rcp-handle-directory-files: couldn't `cd %s'"
         (rcp-shell-quote-argument path))
        (rcp-send-command
         multi-method method user host
         (concat (rcp-get-ls-command multi-method method user host)
                 " -a | cat"))
        (rcp-wait-for-output)
        (goto-char (point-max))
        (while (zerop (forward-line -1))
          (setq x (buffer-substring (point)
                                    (progn (end-of-line) (point))))
          (when (or (not match) (string-match match x))
            (if full
                (push (concat (file-name-as-directory directory)
                              x)
                      result)
              (push x result))))))
    result))

;; This function should return "foo/" for directories and "bar" for
;; files.  We use `ls -ad *' to get a list of files (including
;; directories), and `ls -ad */' to get a list of directories.
(defun rcp-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for rcp files."
  (let ((v (rcp-dissect-file-name (rcp-handle-expand-file-name directory)))
        multi-method method user host path dirs result)
    (setq multi-method (rcp-file-name-multi-method v))
    (setq method (rcp-file-name-method v))
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (rcp-send-command multi-method method user host
                        (format "cd %s ; echo $?"
                                (rcp-shell-quote-argument path)))
      (rcp-barf-unless-okay
       "rcp-handle-file-name-all-completions: Couldn't `cd %s'"
       (rcp-shell-quote-argument path))
      ;; Get list of file names by calling ls.
      (rcp-send-command
       multi-method method user host
       (format "%s -a 2>/dev/null | cat"
               (rcp-get-ls-command multi-method method user host)))
      (rcp-wait-for-output)
      (goto-char (point-max))
      (while (zerop (forward-line -1))
        (push (buffer-substring (point)
                                (progn (end-of-line) (point)))
              result))
      ;; Now get a list of directories in a similar way.
      (rcp-send-command
       multi-method method user host
       (format "%s -d .*/ */ 2>/dev/null | cat"
               (rcp-get-ls-command multi-method method user host)
               (rcp-shell-quote-argument filename)))
      (rcp-wait-for-output)
      (goto-char (point-max))
      (while (zerop (forward-line -1))
        (push (buffer-substring (point)
                                (progn (end-of-line)
                                       (skip-chars-backward "/")
                                       (point)))
              dirs)))
    ;; Now annotate all dirs in list of file names with a slash,
    ;; at the same time checking for 
    (mapcar
     (function (lambda (x)
                 (if (member x dirs)
                     (file-name-as-directory x)
                   x)))
     (save-match-data
       (all-completions
        filename (mapcar 'list result)
        (lambda (x)
          (and (not (string= (car x) "."))
               (not (string= (car x) ".."))
               (not (string-match
                     (concat "\\("
                             (mapconcat 'regexp-quote
                                        completion-ignored-extensions
                                        "\\|")
                             "\\)\\'")
                     (car x))))))))))

;; The following isn't needed for Emacs 20 but for 19.34?
(defun rcp-handle-file-name-completion (filename directory)
  "Like `file-name-completion' for rcp files."
  (unless (rcp-rcp-file-p directory)
    (error "rcp-handle-file-name-completion invoked on non-rcp directory `%s'"
           directory))
  (try-completion
   filename
   (mapcar (lambda (x) (cons x nil))
           (rcp-handle-file-name-all-completions filename directory))))

;; cp, mv and ln

(defun rcp-handle-add-name-to-file
  (filename newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for rcp files."
  (let* ((v1 (when (rcp-rcp-file-p filename)
               (rcp-dissect-file-name (rcp-handle-expand-file-name filename))))
         (v2 (when (rcp-rcp-file-p newname)
               (rcp-dissect-file-name (rcp-handle-expand-file-name newname))))
         (mmeth1 (when v1 (rcp-file-name-multi-method v1)))
         (mmeth2 (when v2 (rcp-file-name-multi-method v2)))
         (meth1  (when v1 (rcp-file-name-method v1)))
         (meth2  (when v2 (rcp-file-name-method v2)))
         (user1  (when v1 (rcp-file-name-user v1)))
         (user2  (when v2 (rcp-file-name-user v2)))
         (host1  (when v1 (rcp-file-name-host v1)))
         (host2  (when v2 (rcp-file-name-host v2)))
         (path1  (when v1 (rcp-file-name-path v1)))
         (path2  (when v2 (rcp-file-name-path v2))))
    (unless (and meth1 meth2 user1 user2 host1 host2
                 (equal mmeth1 mmeth2)
                 (equal meth1 meth2)
                 (equal user1 user2)
                 (equal host1 host2))
      (error "add-name-to-file: %s"
             "only implemented for same method, same user, same host"))
    (when (and (not ok-if-already-exists)
               (file-exists-p newname)
               (not (numberp ok-if-already-exists))
               (y-or-n-p
                (format
                 "File %s already exists; make it a new name anyway? "
                 newname)))
      (error "add-name-to-file: file %s already exists" newname))
    (rcp-send-command mmeth1 meth1 user1 host1
                      (format "ln %s %s ; echo $?"
                              (rcp-shell-quote-argument path1)
                              (rcp-shell-quote-argument path2)))
    (rcp-barf-unless-okay
     "error with add-name-to-file, see buffer `%s' for details"
     (buffer-name))))

(defun rcp-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date)
  "Like `copy-file' for rcp files."
  ;; Check if both files are local -- invoke normal copy-file.
  ;; Otherwise, use rcp from local system.
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  ;; At least one file an rcp file?
  (if (or (rcp-rcp-file-p filename)
          (rcp-rcp-file-p newname))
      (rcp-do-copy-or-rename-file
       'copy filename newname ok-if-already-exists keep-date)
    (rcp-run-real-handler
     'copy-file
     (list filename newname ok-if-already-exists keep-date))))

(defun rcp-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for rcp files."
  ;; Check if both files are local -- invoke normal rename-file.
  ;; Otherwise, use rcp from local system.
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  ;; At least one file an rcp file?
  (if (or (rcp-rcp-file-p filename)
          (rcp-rcp-file-p newname))
      (rcp-do-copy-or-rename-file
       'rename filename newname ok-if-already-exists)
    (rcp-run-real-handler 'rename-file
                          (list filename newname ok-if-already-exists))))

(defun rcp-do-copy-or-rename-file
  (op filename newname &optional ok-if-already-exists keep-date)
  "Copy or rename a remote file.
OP must be `copy' or `rename' and indicates the operation to perform.
FILENAME specifies the file to copy or rename, NEWNAME is the name of
the new file (for copy) or the new name of the file (for rename).
OK-IF-ALREADY-EXISTS means don't barf if NEWNAME exists already.
KEEP-DATE means to make sure that NEWNAME has the same timestamp
as FILENAME.

This function is invoked by `rcp-handle-copy-file' and
`rcp-handle-rename-file'.  It is an error if OP is neither of `copy'
and `rename'.  FILENAME and NEWNAME must be absolute file names."
  (unless (memq op '(copy rename))
    (error "Unknown operation `%s', must be `copy' or `rename'" op))
  (unless ok-if-already-exists
    (when (file-exists-p newname)
      (signal 'file-already-exists
              (list newname))))
  (let* ((v1 (when (rcp-rcp-file-p filename)
               (rcp-dissect-file-name (rcp-handle-expand-file-name filename))))
         (v2 (when (rcp-rcp-file-p newname)
               (rcp-dissect-file-name (rcp-handle-expand-file-name newname))))
         (mmeth1 (when v1 (rcp-file-name-multi-method v1)))
         (mmeth2 (when v2 (rcp-file-name-multi-method v2)))
         (meth1 (when v1 (rcp-file-name-method v1)))
         (meth2 (when v2 (rcp-file-name-method v2)))
         (mmeth (rcp-file-name-multi-method (or v1 v2)))
         (meth (rcp-file-name-method (or v1 v2)))
         (rcp-program (rcp-get-rcp-program mmeth meth))
         (rcp-args (rcp-get-rcp-args mmeth meth))
         (rcpbuf (get-buffer-create "*rcp output*")))
    ;; Check if we can use a shortcut.
    (if (and meth1 meth2 (equal mmeth1 mmeth2) (equal meth1 meth2)
             (equal (rcp-file-name-host v1)
                    (rcp-file-name-host v2))
             (equal (rcp-file-name-user v1)
                    (rcp-file-name-user v2)))
        ;; Shortcut: if method, host, user are the same for both
        ;; files, we invoke `cp' or `mv' on the remote host directly.
        (rcp-do-copy-or-rename-file-directly
         op
         (rcp-file-name-multi-method v1)
         (rcp-file-name-method v1)
         (rcp-file-name-user v1)
         (rcp-file-name-host v1)
         (rcp-file-name-path v1) (rcp-file-name-path v2)
         keep-date)
      ;; New algorithm: copy file first.  Then, if operation is
      ;; `rename', go back and delete the original file if the copy
      ;; was successful.
      (if rcp-program
          ;; The following code uses an rcp program to copy the file.
          (let ((f1 (if (not v1)
                        filename
                      (rcp-make-rcp-program-file-name
                       (rcp-file-name-user v1)
                       (rcp-file-name-host v1)
                       (rcp-shell-quote-argument (rcp-file-name-path v1)))))
                (f2 (if (not v2)
                        newname
                      (rcp-make-rcp-program-file-name
                       (rcp-file-name-user v2)
                       (rcp-file-name-host v2)
                       (rcp-shell-quote-argument (rcp-file-name-path v2)))))
                (default-directory
                  (if (rcp-rcp-file-p default-directory)
                      (rcp-temporary-file-directory)
                    default-directory)))
            (when keep-date
              (add-to-list 'rcp-args (rcp-get-rcp-keep-date-arg mmeth meth)))
            (save-excursion (set-buffer rcpbuf) (erase-buffer))
            (unless
                (equal 0 (apply #'call-process (rcp-get-rcp-program mmeth meth)
                                nil rcpbuf nil (append rcp-args (list f1 f2))))
              (pop-to-buffer rcpbuf)
              (error (concat "rcp-do-copy-or-rename-file: %s"
                             " didn't work, see buffer `%s' for details")
                     (rcp-get-rcp-program mmeth meth) rcpbuf)))
        ;; The following code uses an inline method for copying.
        ;; Let's start with a simple-minded approach: we create a new
        ;; buffer, insert the contents of the source file into it,
        ;; then write out the buffer.  This should work fine, whether
        ;; the source or the target files are rcp files.
        ;; CCC TODO: error checking
        (when keep-date
          (rcp-message 1 (concat "Warning: cannot preserve file time stamp"
				 " with inline copying across machines")))
        (save-excursion
          (set-buffer rcpbuf) (erase-buffer)
          (insert-file-contents-literally filename)
	  (let ((coding-system-for-write 'no-conversion))
	    (write-region (point-min) (point-max) newname)))
        ;; If the operation was `rename', delete the original file.
        (unless (eq op 'copy)
          (delete-file filename))))))

(defun rcp-do-copy-or-rename-file-directly
  (op multi-method method user host path1 path2 keep-date)
  "Invokes `cp' or `mv' on the remote system.
OP must be one of `copy' or `rename', indicating `cp' or `mv',
respectively.  METHOD, USER, and HOST specify the connection.
PATH1 and PATH2 specify the two arguments of `cp' or `mv'.
If KEEP-DATE is non-nil, preserve the time stamp when copying."
  ;; CCC: What happens to the timestamp when renaming?
  (let ((cmd (cond ((and (eq op 'copy) keep-date) "cp -f -p")
                   ((eq op 'copy) "cp -f")
                   ((eq op 'rename) "mv -f")
                   (t (error
                       "Unknown operation `%s', must be `copy' or `rename'"
                       op)))))
    (save-excursion
      (rcp-send-command
       multi-method method user host
       (format "%s %s %s ; echo $?"
               cmd
               (rcp-shell-quote-argument path1)
               (rcp-shell-quote-argument path2)))
      (rcp-barf-unless-okay
       "Copying directly failed, see buffer `%s' for details."
       (buffer-name)))))

;; mkdir
(defun rcp-handle-make-directory (dir &optional parents)
  "Like `make-directory' for rcp files."
  (let ((v (rcp-dissect-file-name (rcp-handle-expand-file-name dir))))
    (rcp-send-command
     (rcp-file-name-multi-method v) (rcp-file-name-method v)
     (rcp-file-name-user v) (rcp-file-name-host v)
     (format "%s %s ; echo $?"
             (if parents "mkdir -p" "mkdir")
             (rcp-shell-quote-argument (rcp-file-name-path v))))
    (rcp-barf-unless-okay "Couldn't make directory %s" dir)))

;; CCC error checking?
(defun rcp-handle-delete-directory (directory)
  "Like `delete-directory' for rcp files."
  (let ((v (rcp-dissect-file-name (rcp-handle-expand-file-name directory))))
    (save-excursion
      (rcp-send-command
       (rcp-file-name-multi-method v) (rcp-file-name-method v)
       (rcp-file-name-user v) (rcp-file-name-host v)
       (format "rmdir %s ; echo ok"
               (rcp-shell-quote-argument (rcp-file-name-path v))))
      (rcp-wait-for-output))))

(defun rcp-handle-delete-file (filename)
  "Like `delete-file' for rcp files."
  (let ((v (rcp-dissect-file-name (rcp-handle-expand-file-name filename))))
    (save-excursion
      (rcp-send-command
       (rcp-file-name-multi-method v)
       (rcp-file-name-method v)
       (rcp-file-name-user v)
       (rcp-file-name-host v)
       (format "rm -f %s ; echo ok"
               (rcp-shell-quote-argument (rcp-file-name-path v))))
      (rcp-wait-for-output))))

;; Dired.

;; CCC: This does not seem to be enough. Something dies when
;;      we try and delete two directories under RCP :/
(defun rcp-handle-dired-recursive-delete-directory (filename)
  "Recursively delete the directory given.
This is like `dired-recursive-delete-directory' for rcp files."
  (let* ((v	 (rcp-dissect-file-name (rcp-handle-expand-file-name filename)))
         (multi-method (rcp-file-name-multi-method v))
	 (method (rcp-file-name-method v))
	 (user   (rcp-file-name-user v))
	 (host   (rcp-file-name-host v))
	 (path   (rcp-file-name-path v)))
    ;; run a shell command 'rm -r <path>'
    ;; Code shamelessly stolen for the dired implementation and, um, hacked :)
    (or (rcp-handle-file-exists-p filename)
	(signal
	 'file-error
	 (list "Removing old file name" "no such directory" filename)))
    ;; Which is better, -r or -R? (-r works for me <daniel@danann.net>)
    (rcp-send-command multi-method method user host 
		      (format "rm -r %s" (rcp-shell-quote-argument path)))
    ;; Wait for the remote system to return to us...
    ;; This might take a while, allow it plenty of time.
    (rcp-wait-for-output 120)
    ;; Make sure that it worked...
    (and (rcp-handle-file-exists-p filename)
	 (error "Failed to recusively delete %s" filename))))
	 

(defun rcp-handle-dired-call-process (program discard &rest arguments)
  "Like `dired-call-process' for rcp files."
  (let ((v (rcp-dissect-file-name
            (rcp-handle-expand-file-name default-directory)))
        multi-method method user host path)
    (setq multi-method (rcp-file-name-multi-method v))
    (setq method (rcp-file-name-method v))
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (rcp-send-command multi-method method user host
                        (format "cd %s ; echo $?"
                                (rcp-shell-quote-argument path)))
      (rcp-barf-unless-okay
       "rcp-handle-dired-call-process: Couldn't `cd %s'"
       (rcp-shell-quote-argument path))
      (rcp-send-command multi-method method user host
                        (mapconcat #'identity (cons program arguments) " "))
      (rcp-wait-for-output))
    (unless discard
      (insert-buffer (rcp-get-buffer multi-method method user host)))
    (save-excursion
      (rcp-send-command multi-method method user host "echo $?")
      (rcp-wait-for-output)
      (read (rcp-get-buffer multi-method method user host)))))

;; Pacify byte-compiler.  The function is needed on XEmacs only.  I'm
;; not sure at all that this is the right way to do it, but let's hope
;; it works for now, and wait for a guru to point out the Right Way to
;; achieve this.
;;(eval-when-compile
;;  (unless (fboundp 'dired-insert-set-properties)
;;    (fset 'dired-insert-set-properties 'ignore)))
;; Gerd suggests this:
(eval-when-compile (require 'dired))
;; Note that dired is required at run-time, too, when it is needed.
;; It is only needed on XEmacs for the function
;; `dired-insert-set-properties'.

(defun rcp-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for rcp files."
  (let ((v (rcp-dissect-file-name (rcp-handle-expand-file-name filename)))
         multi-method method user host path)
    (setq multi-method (rcp-file-name-multi-method v))
    (setq method (rcp-file-name-method v))
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (when (listp switches)
      (setq switches (mapconcat #'identity switches " ")))
    (unless full-directory-p
      (setq switches (concat "-d " switches)))
    (save-excursion
      (if (not (file-directory-p filename))
          ;; Just do `ls -l /tmp/foo' for files.
          (rcp-send-command
           multi-method method user host
           (format "%s %s %s"
                   (rcp-get-ls-command multi-method method user host)
                   switches
                   (rcp-shell-quote-argument path)))
        ;; Do `cd /dir' then `ls -l' for directories.
        (rcp-send-command
         multi-method method user host
         (format "cd %s ; echo $?" (rcp-shell-quote-argument path)))
        (rcp-barf-unless-okay
         "rcp-handle-insert-directory: Couldn't `cd %s'"
         (rcp-shell-quote-argument path))
        (rcp-send-command
         multi-method method user host
         (format "%s %s"
                 (rcp-get-ls-command multi-method method user host)
                 switches)))
      (sit-for 1)                       ;needed for rsh but not ssh?
      (rcp-wait-for-output))
    (insert-buffer (rcp-get-buffer multi-method method user host))
    ;; On XEmacs, we want to call (exchange-point-and-mark t), but
    ;; that doesn't exist on Emacs, so we use this workaround instead.
    ;; Since zmacs-region-stays doesn't exist in Emacs, this ought to
    ;; be safe.  Thanks to Daniel Pittman <daniel@danann.net>.
    (let ((zmacs-region-stays t))
      (exchange-point-and-mark))
    ;; Another XEmacs specialty follows.  What's the right way to do
    ;; it?
    (when (and (featurep 'xemacs)
               (eq major-mode 'dired-mode))
      (save-excursion
        (require 'dired)
        (dired-insert-set-properties (point) (mark t))))))

;; Continuation of kluge to pacify byte-compiler.
;;(eval-when-compile
;;  (when (eq (symbol-function 'dired-insert-set-properties) 'ignore)
;;    (fmakunbound 'dired-insert-set-properties)))

;; CCC is this the right thing to do?
(defun rcp-handle-unhandled-file-name-directory (filename)
  "Like `unhandled-file-name-directory' for rcp files."
  (expand-file-name "~/"))

;; Canonicalization of file names.

(defun rcp-drop-volume-letter (name)
  "Cut off unnecessary drive letter from file NAME.
The function `rcp-handle-expand-file-name' calls `expand-file-name'
locally on a remote file name.  When the local system is a W32 system
but the remote system is Unix, this introduces a superfluous drive
letter into the file name.  This function removes it.

Doesn't do anything if the NAME does not start with a drive letter."
  (if (and (> (length name) 1)
           (char-equal (aref name 1) ?:)
           (let ((c1 (aref name 0)))
             (or (and (>= c1 ?A) (<= c1 ?Z))
                 (and (>= c1 ?a) (<= c1 ?z)))))
      (substring name 2)
    name))

(defun rcp-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for rcp files."
  (save-match-data
    ;; If DIR is not given, use DEFAULT-DIRECTORY or "/".
    (setq dir (or dir default-directory "/"))
    ;; Unless NAME is absolute, concat DIR and NAME.
    (unless (file-name-absolute-p name)
      (setq name (concat (file-name-as-directory dir) name)))
    ;; If NAME is not an rcp file, run the real handler
    (if (not (rcp-rcp-file-p name))
        (rcp-run-real-handler 'expand-file-name
                              (list name nil))
      ;; Dissect NAME.
      (let* ((v (rcp-dissect-file-name name))
             (multi-method (rcp-file-name-multi-method v))
             (method (rcp-file-name-method v))
             (user (rcp-file-name-user v))
             (host (rcp-file-name-host v))
             (path (rcp-file-name-path v)))
        (unless (file-name-absolute-p path)
          (setq path (concat "~/" path)))
        (save-excursion
          ;; Tilde expansion if necessary.  This needs a shell which
          ;; groks tilde expansion!  The function `rcp-find-shell' is
          ;; supposed to find such a shell on the remote host.  Please
          ;; tell me about it when this doesn't work on your system.
          (when (string-match "\\`\\(~[^/]*\\)\\(.*\\)\\'" path)
            (let ((uname (match-string 1 path))
                  (fname (match-string 2 path)))
              ;; CCC fanatic error checking?
              (rcp-send-command
               multi-method method user host
               (format "cd %s; pwd" uname))
              (rcp-wait-for-output)
              (goto-char (point-min))
              ;; Use of (progn (end-of-line) (point)) instead of
              ;; (line-end-position) not necessary?
              (setq uname (buffer-substring (point) (line-end-position)))
              (setq path (concat uname fname)))) ;)
          ;; No tilde characters in file name, do normal
          ;; expand-file-name (this does "/./" and "/../").
          (rcp-make-rcp-file-name
           multi-method method user host
           (rcp-drop-volume-letter
            (rcp-run-real-handler 'expand-file-name (list path)))))))))

;; Remote commands.

(defun rcp-handle-shell-command (command &optional output-buffer error-buffer)
  "Like `shell-command' for rcp files.
This will break if COMMAND prints a newline, followed by the value of
`rcp-end-of-output', followed by another newline."
  (if (rcp-rcp-file-p default-directory)
      (let* ((v (rcp-dissect-file-name
                 (rcp-handle-expand-file-name default-directory)))
             (multi-method (rcp-file-name-multi-method v))
             (method (rcp-file-name-method v))
             (user (rcp-file-name-user v))
             (host (rcp-file-name-host v))
             (path (rcp-file-name-path v)))
        (save-match-data
          (when (string-match "&[ \t]*\\'" command)
            (error "Rcp doesn't grok asynchronous shell commands, yet")))
        (when error-buffer
          (error "Rcp doesn't grok optional third arg ERROR-BUFFER, yet"))
        (save-excursion
          (rcp-send-command
           multi-method method user host
           (format "cd %s; echo $?" (rcp-shell-quote-argument path)))
          (rcp-barf-unless-okay
           "rcp-handle-shell-command: Couldn't `cd %s'"
           (rcp-shell-quote-argument path))
          (rcp-send-command multi-method method user host
                            (concat command "; rcp_old_status=$?"))
          ;; This will break if the shell command prints "/////"
          ;; somewhere.  Let's just hope for the best...
          (rcp-wait-for-output))
        (unless output-buffer
          (setq output-buffer (get-buffer-create "*Shell Command Output*"))
          (set-buffer output-buffer)
          (erase-buffer))
        (unless (bufferp output-buffer)
          (setq output-buffer (current-buffer)))
        (set-buffer output-buffer)
        (insert-buffer (rcp-get-buffer multi-method method user host))
        (save-excursion
          (rcp-send-command
           multi-method method user host
           "rcp_set_exit_status $rcp_old_status")
          (rcp-wait-for-output))
        (unless (zerop (buffer-size))
          (pop-to-buffer output-buffer)))
    ;; The following is only executed if something strange was
    ;; happening.  Emit a helpful message and do it anyway.
    (message "rcp-handle-shell-command called with non-rcp directory: `%s'"
             default-directory)
    (rcp-run-real-handler 'shell-command
                          (list command output-buffer error-buffer))))

;; File Editing.

(defsubst rcp-make-temp-file ()
  (funcall (if (fboundp 'make-temp-file) 'make-temp-file 'make-temp-name)
	   (expand-file-name rcp-temp-name-prefix
			     (rcp-temporary-file-directory))))

(defun rcp-handle-file-local-copy (filename)
  "Like `file-local-copy' for rcp files."
  (let* ((v (rcp-dissect-file-name (rcp-handle-expand-file-name filename)))
         (multi-method (rcp-file-name-multi-method v))
         (method (rcp-file-name-method v))
         (user (rcp-file-name-user v))
         (host (rcp-file-name-host v))
         (path (rcp-file-name-path v))
         (rcpbuf (get-buffer-create "*rcp output*"))
         tmpfil)
    (unless (file-exists-p filename)
      (error "Cannot make local copy of non-existing file `%s'"
             filename))
    (setq tmpfil (rcp-make-temp-file))
    (cond ((rcp-get-rcp-program multi-method method)
           ;; Use rcp-like program for file transfer.
           (rcp-message 5 "Fetching %s to tmp file..." filename)
           (save-excursion (set-buffer rcpbuf) (erase-buffer))
           (unless (equal 0
                          (apply #'call-process
                                 (rcp-get-rcp-program multi-method method)
                                 nil rcpbuf nil
                                 (append (rcp-get-rcp-args multi-method method)
                                         (list
                                          (rcp-make-rcp-program-file-name
                                           user host
                                           (rcp-shell-quote-argument path))
                                          tmpfil))))
             (pop-to-buffer rcpbuf)
             (error (concat "rcp-handle-file-local-copy: `%s' didn't work, "
                            "see buffer `%s' for details")
                    (rcp-get-rcp-program multi-method method) rcpbuf))
           (rcp-message 5 "Fetching %s to tmp file...done" filename))
          ((and (rcp-get-encoding-command multi-method method)
                (rcp-get-decoding-command multi-method method))
           ;; Use inline encoding for file transfer.
           (save-excursion
             ;; Following line for setting rcp-current-method,
             ;; rcp-current-user, rcp-current-host.
             (set-buffer (rcp-get-buffer multi-method method user host))
             (rcp-message 5 "Encoding remote file %s..." filename)
             (rcp-send-command
              multi-method method user host
              (concat (rcp-get-encoding-command multi-method method)
                      " < " (rcp-shell-quote-argument path)
                      "; echo $?"))
             (rcp-barf-unless-okay
              "Encoding remote file failed, see buffer `%s' for details"
              (rcp-get-buffer multi-method method user host))
             ;; Remove trailing status code
             (goto-char (point-max))
             (delete-region (point) (progn (forward-line -1) (point)))

             (rcp-message 5 "Decoding remote file %s..." filename)
             (if (and (rcp-get-decoding-function multi-method method)
                      (fboundp (rcp-get-decoding-function multi-method method)))
                 ;; If rcp-decoding-function is defined for this
                 ;; method, we call it.
                 (let ((tmpbuf (get-buffer-create " *rcp tmp*")))
                   (set-buffer tmpbuf)
                   (erase-buffer)
                   (insert-buffer (rcp-get-buffer multi-method method
                                                  user host))
                   (rcp-message-for-buffer
                    multi-method method user host
                    6 "Decoding remote file %s with function %s..."
                    filename
                    (rcp-get-decoding-function multi-method method))
                   (set-buffer tmpbuf)
                   (funcall (rcp-get-decoding-function multi-method method)
                            (point-min)
                            (point-max))
                   (let ((coding-system-for-write 'no-conversion))
		     (write-region (point-min) (point-max) tmpfil))
                   (kill-buffer tmpbuf))
               ;; If rcp-decoding-function is not defined for this
               ;; method, we invoke rcp-decoding-command instead.
             (let ((tmpfil2 (rcp-make-temp-file)))
               (write-region (point-min) (point-max) tmpfil2)
               (rcp-message
                6 "Decoding remote file %s with command %s..."
                filename
                (rcp-get-decoding-command multi-method method))
               (call-process
                rcp-sh-program
                tmpfil2                 ;input
                nil                     ;output
                nil                     ;display
                "-c" (concat (rcp-get-decoding-command multi-method method)
                             " > " tmpfil))
               (delete-file tmpfil2)))
             (rcp-message-for-buffer
              multi-method method user host
              5 "Decoding remote file %s...done" filename)))

          (t (error "Wrong method specification for `%s'" method)))
    tmpfil))


(defun rcp-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for rcp files."
  (if (not (rcp-handle-file-exists-p filename))
      (progn
        (when visit
          (setq buffer-file-name filename)
          (set-visited-file-modtime '(0 0))
          (set-buffer-modified-p nil))
	(signal 'file-error
                (format "File `%s' not found on remote host" filename))
        (list (rcp-handle-expand-file-name filename) 0))
    (let ((local-copy (rcp-handle-file-local-copy filename))
          (result nil))
      (when visit
        (setq buffer-file-name filename)
        (set-visited-file-modtime '(0 0))
        (set-buffer-modified-p nil))
      (setq result
            (let ((coding-system-for-read 'no-conversion))
	      (rcp-run-real-handler 'insert-file-contents
				    (list local-copy nil beg end replace))))
      (delete-file local-copy)
      (list (expand-file-name filename)
            (second result)))))

;; CCC grok APPEND, LOCKNAME, CONFIRM
(defun rcp-handle-write-region
  (start end filename &optional append visit lockname confirm)
  "Like `write-region' for rcp files."
  (unless (eq append nil)
    (error "Cannot append to file using rcp (`%s')" filename))
  (unless (or (eq lockname nil)
              (string= lockname filename))
    (error "rcp-handle-write-region: LOCKNAME must be nil or equal FILENAME"))
  ;; XEmacs takes a coding system as the sevent argument, not `confirm'
  (when (and (not (featurep 'xemacs))
		  confirm (file-exists-p filename))
    (unless (y-or-n-p (format "File %s exists; overwrite anyway? "
                              filename))
      (error "File not overwritten")))
  (let* ((v (rcp-dissect-file-name filename))
         (multi-method (rcp-file-name-multi-method v))
         (method (rcp-file-name-method v))
         (user (rcp-file-name-user v))
         (host (rcp-file-name-host v))
         (path (rcp-file-name-path v))
         (rcp-program (rcp-get-rcp-program multi-method method))
         (rcp-args (rcp-get-rcp-args multi-method method))
         (encoding-command (rcp-get-encoding-command multi-method  method))
         (encoding-function (rcp-get-encoding-function multi-method method))
         (decoding-command (rcp-get-decoding-command multi-method method))
         (rcpbuf (get-buffer-create "*rcp output*"))
         ;; We use this to save the value of `last-coding-system-used'
         ;; after writing the tmp file.  At the end of the function,
         ;; we set `last-coding-system-used' to this saved value.
         ;; This way, any intermediary coding systems used while
         ;; talking to the remote shell or suchlike won't hose this
         ;; variable.  This approach was snarfed from ange-ftp.el.
         coding-system-used
         tmpfil)
    ;; Write region into a tmp file.  This isn't really needed if we
    ;; use an encoding function, but currently we use it always
    ;; because this makes the logic simpler.
    (setq tmpfil (rcp-make-temp-file))
    (rcp-run-real-handler
     'write-region
     (if confirm ; don't pass this arg unless defined for backward compat.
         (list start end tmpfil append 'no-message lockname confirm)
       (list start end tmpfil append 'no-message lockname)))
    ;; Now, `last-coding-system-used' has the right value.  Remember it.
    (when (boundp 'last-coding-system-used)
      (setq coding-system-used last-coding-system-used))
    ;; This is a bit lengthy due to the different methods possible for
    ;; file transfer.  First, we check whether the method uses an rcp
    ;; program.  If so, we call it.  Otherwise, both encoding and
    ;; decoding command must be specified.  However, if the method
    ;; _also_ specifies an encoding function, then that is used for
    ;; encoding the contents of the tmp file.
    (cond (rcp-program
           ;; use rcp-like program for file transfer
           (rcp-message-for-buffer
            multi-method method user host
            6 "Writing tmp file using `%s'..." rcp-program)
           (save-excursion (set-buffer rcpbuf) (erase-buffer))
           (unless (equal 0
                          (apply #'call-process
                                 rcp-program nil rcpbuf nil
                                 (append rcp-args
                                         (list
                                          tmpfil
                                          (rcp-make-rcp-program-file-name
                                           user host
                                           (rcp-shell-quote-argument path))))))
             (pop-to-buffer rcpbuf)
             (error "Cannot write region to file `%s', command `%s' failed"
                     filename rcp-program))
           (rcp-message-for-buffer multi-method method user host
                                   6 "Transferring file using `%s'...done"
                                   rcp-program))
          ((and encoding-command decoding-command)
           ;; Use inline file transfer
           (let ((tmpbuf (get-buffer-create " *rcp file transfer*")))
             (save-excursion
               ;; Encode tmpfil into tmpbuf
               (rcp-message-for-buffer multi-method method user host
                                       5 "Encoding region...")
               (set-buffer tmpbuf)
               (erase-buffer)
               ;; Use encoding function or command.
               (if (and encoding-function
                        (fboundp encoding-function))
                   (progn
                     (rcp-message-for-buffer
                      multi-method method user host
                      6 "Encoding region using function...")
                     (insert-file-contents-literally tmpfil)
                     ;; CCC.  The following `let' is a workaround for
                     ;; the base64.el that comes with pgnus-0.84.  If
                     ;; both of the following conditions are
                     ;; satisfied, it tries to write to a local file
                     ;; in default-directory, but at this point,
                     ;; default-directory is remote.
                     ;; (CALL-PROCESS-REGION can't write to remote
                     ;; files, it seems.)  The file in question is a
                     ;; tmp file anyway.
                     (let ((default-directory (rcp-temporary-file-directory)))
                       (funcall encoding-function (point-min) (point-max)))
                     (goto-char (point-max))
                     (unless (bolp)
                       (newline)))
                 (rcp-message-for-buffer multi-method method user host
                                         6 "Encoding region using command...")
                 (unless (equal 0
                                (call-process
                                 rcp-sh-program
                                 tmpfil ;input = local tmp file
                                 t      ;output is current buffer
                                 nil    ;don't redisplay
                                 "-c"
                                 encoding-command))
                   (pop-to-buffer rcpbuf)
                   (error (concat "Cannot write to `%s', local encoding"
                                  " command `%s' failed")
                          filename encoding-command)))
               ;; Send tmpbuf into remote decoding command which
               ;; writes to remote file.  Because this happens on the
               ;; remote host, we cannot use the function.
               (rcp-message-for-buffer
                multi-method method user host
                5 "Decoding region into remote file %s..." filename)
               (rcp-send-command
                multi-method method user host
                (format "%s <<'%s' >%s" ;mkoeppe: must quote EOF delimiter
                        decoding-command
                        rcp-end-of-output
                        (rcp-shell-quote-argument path)))
               (set-buffer tmpbuf)
               (rcp-message-for-buffer
                multi-method method user host
                6 "Sending data to remote host...")
               (rcp-send-region multi-method method user host
                                (point-min) (point-max))
               ;; wait for remote decoding to complete
               (rcp-message-for-buffer
                multi-method method user host 6 "Sending end of data token...")
               (rcp-send-command multi-method method user host
                                 rcp-end-of-output t)
               (rcp-message 6 "Waiting for remote host to process data...")
               ;;(rcp-send-command multi-method method user host "echo hello")
               ;;(set-buffer (rcp-get-buffer multi-method method user host))
               (rcp-wait-for-output)
               (rcp-send-command multi-method method user host "echo $?")
               (rcp-barf-unless-okay
                (concat "Couldn't write region to `%s',"
                        " decode using `%s' failed")
                filename decoding-command)
               (rcp-message 5 "Decoding region into remote file %s...done"
                            filename)
               (kill-buffer tmpbuf))))
          (t
           (error
            (concat "Method `%s' should specify both encoding and "
                    "decoding command or an rcp program")
            method)))
    (delete-file tmpfil)
    ;; Make `last-coding-system-used' have the right value.
    (when (boundp 'last-coding-system-used)
      (setq last-coding-system-used coding-system-used))
    (when (or (eq visit t)
              (eq visit nil)
              (stringp visit))
      (message "Wrote %s" filename))))

;; Call down to the real handler.
;; Because EFS does not play nicely with RCP (both systems match an
;; RCP path) it is needed to disable efs as well as rcp for the
;; operation.
;;
;; Other than that, this is the canon file-handler code that the doco
;; says should be used here. Which is nice.
;;
;; Under XEmacs current, EFS also hooks in as
;; efs-sifn-handler-function to handle any path with environment
;; variables. This has two implications:
;; 1) That EFS may not be completely dead (yet) for RCP paths
;; 2) That RCP might want to do the same thing.
;; Details as they come in.
;;
;; Daniel Pittman <daniel@danann.net>
(defun rcp-run-real-handler (operation args)
  "Invoke normal file name handler for OPERATION.
This inhibits EFS and Ange-FTP, too, because they conflict with rcp.
First arg specifies the OPERATION, remaining ARGS are passed to the
OPERATION."
  (let ((inhibit-file-name-handlers
         (list 'rcp-file-name-handler
	       'efs-file-handler-function
               'ange-ftp-hook-function
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))


;; Main function.
(defun rcp-file-name-handler (operation &rest args)
  "Invoke rcp file name handler.
Falls back to normal file name handler if no rcp file name handler exists."
  (let ((fn (assoc operation rcp-file-name-handler-alist)))
    ;(message "Handling %s using %s" operation fn)
    (if fn
        (apply (cdr fn) args)
      (rcp-run-real-handler operation args))))

;; Register in file name handler alist

(defun rcp-setup-file-name-handler-alist ()
  (add-to-list 'file-name-handler-alist
               (cons rcp-file-name-regexp 'rcp-file-name-handler)))
(rcp-setup-file-name-handler-alist)

;;; Interactions with other packages:

;; -- complete.el --

;; This function contributed by Ed Sabol
(defun rcp-handle-expand-many-files (name)
  "Like `PC-expand-many-files' for rcp files."
  (save-match-data
    (if (or (string-match "\\*" name)
            (string-match "\\?" name)
            (string-match "\\[.*\\]" name))
        (save-excursion
          ;; Dissect NAME.
          (let* ((v (rcp-dissect-file-name name))
                 (multi-method (rcp-file-name-multi-method v))
                 (method (rcp-file-name-method v))
                 (user (rcp-file-name-user v))
                 (host (rcp-file-name-host v))
                 (path (rcp-file-name-path v))
                 bufstr)
            ;; CCC: To do it right, we should quote certain characters
            ;; in the file name, but since the echo command is going to
            ;; break anyway when there are spaces in the file names, we
            ;; don't bother.
            ;;-(let ((comint-file-name-quote-list
            ;;-       (set-difference rcp-file-name-quote-list
            ;;-                       '(?\* ?\? ?[ ?]))))
            ;;-  (rcp-send-command
            ;;-   multi-method method user host
            ;;-   (format "echo %s" (comint-quote-filename path)))
            ;;-  (rcp-wait-for-output))
            (rcp-send-command multi-method method user host
                              (format "echo %s" path))
            (rcp-wait-for-output)
            (setq bufstr (buffer-substring (point-min)
                                           (progn (end-of-line) (point))))
            (goto-char (point-min))
            (if (string-equal path bufstr)
                nil
              (insert "(\"")
              (while (search-forward " " nil t)
                (delete-backward-char 1)
                (insert "\" \""))
              (goto-char (point-max))
              (delete-backward-char 1)
              (insert "\")")
              (goto-char (point-min))
              (mapcar
               (function (lambda (x)
                           (rcp-make-rcp-file-name multi-method method
                                                   user host x)))
               (read (current-buffer))))))
      (list (rcp-handle-expand-file-name name)))))

;; Check for complete.el and override PC-expand-many-files if appropriate.
(eval-when-compile
  (defun rcp-save-PC-expand-many-files (name))); avoid compiler warning

(defun rcp-setup-complete ()
  (fset 'rcp-save-PC-expand-many-files
        (symbol-function 'PC-expand-many-files))
  (defun PC-expand-many-files (name)
    (if (rcp-rcp-file-p name)
        (rcp-handle-expand-many-files name)
      (rcp-save-PC-expand-many-files name))))

;; Why isn't eval-after-load sufficient?
(if (fboundp 'PC-expand-many-files)
    (rcp-setup-complete)
  (eval-after-load "complete" '(rcp-setup-complete)))

;; -- vc --

;; This used to blow away the file-name-handler-alist and reinstall
;; RCP into it. This was intended to let VC work remotely. It didn't,
;; at least not in my XEmacs 21.2 install.
;; 
;; In any case, rcp-run-real-handler now deals correctly with disabling
;; the things that should be, making this a no-op.
;;
;; I have removed it from the rcp-file-name-handler-alist because the
;; shortened version does nothing. This is for reference only now.
;;
;; Daniel Pittman <daniel@danann.net>
;;
;; (defun rcp-handle-vc-registered (file)
;;   "Like `vc-registered' for rcp files."
;;   (rcp-run-real-handler 'vc-registered (list file)))

;; `vc-do-command'
;; This function does not deal well with remote files, so we define
;; our own version and make a backup of the original function and
;; call our version for rcp files and the original version for
;; normal files.

;; The following function is pretty much copied from vc.el, but
;; the part that actually executes a command is changed.
(defun rcp-vc-do-command (buffer okstatus command file last &rest flags)
  "Like `vc-do-command' but invoked for rcp files.
See `vc-do-command' for more information."
  (and file (setq file (rcp-handle-expand-file-name file)))
  (if (not buffer) (setq buffer "*vc*"))
  (if vc-command-messages
      (message "Running `%s' on `%s'..." command file))
  (let ((obuf (current-buffer)) (camefrom (current-buffer))
	(squeezed nil)
	(olddir default-directory)
	vc-file status)
    (let* ((v (rcp-dissect-file-name (rcp-handle-expand-file-name file)))
           (multi-method (rcp-file-name-multi-method v))
           (method (rcp-file-name-method v))
           (user (rcp-file-name-user v))
           (host (rcp-file-name-host v))
           (path (rcp-file-name-path v)))
      (set-buffer (get-buffer-create buffer))
      (set (make-local-variable 'vc-parent-buffer) camefrom)
      (set (make-local-variable 'vc-parent-buffer-name)
           (concat " from " (buffer-name camefrom)))
      (setq default-directory olddir)
    
      (erase-buffer)

      (mapcar
       (function
        (lambda (s) (and s (setq squeezed (append squeezed (list s))))))
       flags)
      (if (and (eq last 'MASTER) file
               (setq vc-file (vc-name file)))
          (setq squeezed
                (append squeezed
                        (list (rcp-file-name-path
                               (rcp-dissect-file-name vc-file))))))
      (if (and file (eq last 'WORKFILE))
          (progn
            (let* ((pwd (expand-file-name default-directory))
                   (preflen (length pwd)))
              (if (string= (substring file 0 preflen) pwd)
                  (setq file (substring file preflen))))
            (setq squeezed (append squeezed (list file)))))
      ;; Unless we (save-window-excursion) the layout of windows in
      ;; the current frame changes. This is painful, at best.
      ;;
      ;; As a point of note, (save-excursion) is still here only because
      ;; it preserves (point) in the current buffer. (save-window-excursion)
      ;; does not, at least under XEmacs 21.2.
      ;;
      ;; I trust that the FSF support this as well. I can't find useful
      ;; documentation to check :(
      ;;
      ;; Daniel Pittman <daniel@danann.net>
      (save-excursion
	(save-window-excursion
	  ;; Actually execute remote command
	  (rcp-handle-shell-command
	   (mapconcat 'rcp-shell-quote-argument
		      (cons command squeezed) " ") t)
	  ;;(rcp-wait-for-output)
	  ;; Get status from command
	  (rcp-send-command multi-method method user host "echo $?")
	  (rcp-wait-for-output)
          ;; Make sure to get status from last line of output.
          (goto-char (point-max)) (forward-line -1)
	  (setq status (read (current-buffer)))
	  (message "Command %s returned status %d." command status)))
      (goto-char (point-max))
      (set-buffer-modified-p nil)
      (forward-line -1)
      (if (or (not (integerp status)) (and okstatus (< okstatus status)))
          (progn
            (pop-to-buffer buffer)
            (goto-char (point-min))
            (shrink-window-if-larger-than-buffer)
            (error "Running `%s'...FAILED (%s)" command
                   (if (integerp status)
                       (format "status %d" status)
                     status))
            )
        (if vc-command-messages
            (message "Running %s...OK" command))
        )
      (set-buffer obuf)
      status))
  )

;; The context for a VC command is the current buffer.
;; That makes a test on the buffers file more reliable than a test on the
;; arguments.
;; This is needed to handle remote VC correctly - else we test against the
;; local VC system and get things wrong...
;; Daniel Pittman <daniel@danann.net>
(defadvice vc-do-command
  (around rcp-advice-vc-do-command
          (buffer okstatus command file last &rest flags)
          activate)
  "Invoke rcp-vc-do-command for rcp files."
  (let ((file (symbol-value 'file)))
    (if (or (and (stringp file)     (rcp-rcp-file-p file))
            (and (buffer-file-name) (rcp-rcp-file-p (buffer-file-name))))
        (setq ad-return-value
              (apply 'rcp-vc-do-command buffer okstatus command 
                     (or file (buffer-file-name)) last flags))
      ad-do-it)))


;; XEmacs uses this to do some of its work. Like vc-do-command, we
;; need to enhance it to make VC work via RCP-mode.
;;
;; Like the previous function, this is a cut-and-paste job from the VC
;; file. It's based on the vc-do-command code.
(defun rcp-vc-simple-command (okstatus command file &rest args)
  ;; Simple version of vc-do-command, for use in vc-hooks only.
  ;; Don't switch to the *vc-info* buffer before running the
  ;; command, because that would change its default directory
  (let* ((v (rcp-dissect-file-name (rcp-handle-expand-file-name file)))
         (multi-method (rcp-file-name-multi-method v))
	 (method (rcp-file-name-method v))
	 (user (rcp-file-name-user v))
	 (host (rcp-file-name-host v))
	 (path (rcp-file-name-path v)))
    (save-excursion (set-buffer (get-buffer-create "*vc-info*"))
		    (erase-buffer))
    (let ((exec-path (append vc-path exec-path)) exec-status
	  ;; Add vc-path to PATH for the execution of this command.
	  (process-environment
	   (cons (concat "PATH=" (getenv "PATH")
			 path-separator
			 (mapconcat 'identity vc-path path-separator))
		 process-environment)))
      ;; Call the actual process. See rcp-vc-do-command for discussion of
      ;; why this does both (save-window-excursion) and (save-excursion).
      ;;
      ;; As a note, I don't think that the process-environment stuff above
      ;; has any effect on the remote system. This is a hard one though as
      ;; there is no real reason to expect local and remote paths to be
      ;; identical...
      ;;
      ;; Daniel Pittman <daniel@danann.net>
      (save-excursion
	(save-window-excursion
	  ;; Actually execute remote command
	  (rcp-handle-shell-command
	   (mapconcat 'rcp-shell-quote-argument
		      (append (list command) args (list path)) " ")
	   (get-buffer-create"*vc-info*"))
					;(rcp-wait-for-output)
	  ;; Get status from command
	  (rcp-send-command multi-method method user host "echo $?")
	  (rcp-wait-for-output)
	  (setq exec-status (read (current-buffer)))
	  (message "Command %s returned status %d." command exec-status)))
      
      (cond ((> exec-status okstatus)
	     (switch-to-buffer (get-file-buffer file))
	     (shrink-window-if-larger-than-buffer
	      (display-buffer "*vc-info*"))
	     (error "Couldn't find version control information")))
      exec-status)))

(defadvice vc-simple-command
  (around rcp-advice-vc-simple-command
	  (okstatus command file &rest args)
	  activate)
  "Invoke rcp-vc-simple-command for rcp files."
  (let ((file (symbol-value 'file)))
    (if (or (and (stringp file)     (rcp-rcp-file-p file))
            (and (buffer-file-name) (rcp-rcp-file-p (buffer-file-name))))
        (setq ad-return-value
              (apply 'rcp-vc-simple-command okstatus command 
                     (or file (buffer-file-name)) args))
      ad-do-it)))


;; `vc-workfile-unchanged-p'
;; This function does not deal well with remote files, so we do the
;; same as for `vc-do-command'.

;; `vc-workfile-unchanged-p' checks the modification time, we cannot
;; do that for remote files, so here's a version which relies on diff.
(defun rcp-vc-workfile-unchanged-p
  (filename &optional want-differences-if-changed)
  (let ((status (vc-backend-diff filename nil nil
                                 (not want-differences-if-changed))))
    (zerop status)))

(defadvice vc-workfile-unchanged-p
  (around rcp-advice-vc-workfile-unchanged-p
          (filename &optional want-differences-if-changed)
          activate)
  "Invoke rcp-vc-workfile-unchanged-p for rcp files."
  (if (and (stringp filename) (rcp-rcp-file-p filename))
      (setq ad-return-value
            (rcp-vc-workfile-unchanged-p filename want-differences-if-changed))
    ad-do-it))


;; Redefine a function from vc.el -- allow rcp files.
;; `save-match-data' seems not to be required -- it isn't in
;; the original version, either.
(defun vc-checkout (filename &optional writable rev)
  "Retrieve a copy of the latest version of the given file."
  ;; If ftp is on this system and the name matches the ange-ftp format
  ;; for a remote file, the user is trying something that won't work.
  (if (and (not (rcp-rcp-file-p filename))
           (string-match "^/[^/:]+:" filename) (vc-find-binary "ftp"))
      (error "Sorry, you can't check out files over FTP"))
  (vc-backend-checkout filename writable rev)
  (vc-resynch-buffer filename t t))


;; Do we need to advise the vc-user-login-name function anyway?
;; This will return the correct login name for the owner of a 
;; file. It does not deal with the default remote user name...
;;
;; That is, when vc calls (vc-user-login-name), we return the 
;; local login name, something that may be different to the remote
;; default. 
;;
;; The remote VC operations will occur as the user that we logged
;; in with however - not always the same as the local user.
;;
;; In the end, I did advise the function. This is because, well, 
;; the thing didn't work right otherwise ;)
;;
;; Daniel Pittman <daniel@danann.net>

(defun rcp-handle-vc-user-login-name (&optional uid)
  "Return the default user name on the remote machine.
Whenever VC calls this function, `file' is bound to the file name
in question.  If no uid is provided or the uid is equal to the uid
owning the file, then we return the user name given in the file name.

This should only be called when `file' is bound to the
filename we are thinking about..."
  (let ((file (symbol-value 'file)))
    (if (and uid (/= uid (nth 2 (file-attributes file))))
        (error "rcp-handle-vc-user-login-name cannot map a uid to a name")
      (let ((v (rcp-dissect-file-name (rcp-handle-expand-file-name file))))
        (rcp-file-name-user v)))))

(defadvice vc-user-login-name
  (around rcp-vc-user-login-name activate)
  "Support for files on remote machines accessed by RCP."
  ;; We rely on the fact that `file' is bound when this is called.
  ;; This appears to be the case everywhere in vc.el and vc-hooks.el
  ;; as of Emacs 20.5.
  ;;
  ;; CCC TODO there should be a real solution!  Talk to Andre Spiegel
  ;; about this.
  (let ((file (symbol-value 'file)))
    (or (and (stringp file)
             (rcp-rcp-file-p file)      ; rcp file
             (setq ad-return-value 
                   (rcp-handle-vc-user-login-name uid))) ; get the owner name
        ad-do-it)))                     ; else call the original
  
;; Determine the name of the user owning a file.
(defun rcp-file-owner (filename)
  "Return who owns FILE (user name, as a string)."
  (let ((v (rcp-dissect-file-name 
	    (rcp-handle-expand-file-name filename))))
    (if (not (rcp-handle-file-exists-p filename))
        nil                             ; file cannot be opened
      ;; file exists, find out stuff
      (save-excursion
        (rcp-send-command
         (rcp-file-name-multi-method v) (rcp-file-name-method v)
         (rcp-file-name-user v) (rcp-file-name-host v)
         (format "%s -Lld %s"
                 (rcp-get-ls-command (rcp-file-name-multi-method v)
                                     (rcp-file-name-method v)
                                     (rcp-file-name-user v)
                                     (rcp-file-name-host v))
                 (rcp-shell-quote-argument (rcp-file-name-path v))))
        (rcp-wait-for-output)
        ;; parse `ls -l' output ...
        ;; ... file mode flags
        (read (current-buffer))
        ;; ... number links
        (read (current-buffer))
        ;; ... uid (as a string)
        (symbol-name (read (current-buffer)))))))

;; Wire ourselves into the VC infrastructure...
(defadvice vc-file-owner
  (around rcp-vc-file-owner activate)
  "Support for files on remote machines accessed by RCP."
  (let ((filename (ad-get-arg 0)))
    (or (and (rcp-file-name-p filename) ; rcp file
             (setq ad-return-value
                   (rcp-file-owner filename))) ; get the owner name
        ad-do-it)))                     ; else call the original


;; We need to make the version control software backend version
;; information local to the current buffer. This is because each RCP
;; buffer can (theoretically) have a different VC version and I am
;; *way* too lazy to try and push the correct value into each new
;; buffer.
;;
;; Remote VC costs will just have to be paid, at least for the moment.
;; Well, at least, they will right until I feel guilty about doing a
;; botch job here and fix it. :/
;;
;; Daniel Pittman <daniel@danann.net>
(defun rcp-vc-setup-for-remote ()
  "Make the backend release variables buffer local.
This makes remote VC work correctly at the cost of some processing time."
  (when (and (buffer-file-name)
             (rcp-rcp-file-p (buffer-file-name)))
    (make-local-variable 'vc-rcs-release)
    (make-local-variable 'vc-cvs-release)
    (make-local-variable 'vc-sccs-release)
    (setq vc-rcs-release  nil
	  vc-cvs-release  nil
	  vc-sccs-release nil)))
(add-hook 'find-file-hooks 'rcp-vc-setup-for-remote t)


;;; Internal Functions:

(defun rcp-set-auto-save ()
  (when (and (buffer-file-name)
             (rcp-rcp-file-p (buffer-file-name))
             auto-save-default)
    (auto-save-mode 1)))
(add-hook 'find-file-hooks 'rcp-set-auto-save t)

(defun rcp-run-test (switch filename)
  "Run `test' on the remote system, given a SWITCH and a FILENAME.
Returns the exit code of the `test' program."
  (let ((v (rcp-dissect-file-name filename)))
    (save-excursion
      (rcp-send-command
       (rcp-file-name-multi-method v) (rcp-file-name-method v)
       (rcp-file-name-user v) (rcp-file-name-host v)
       (format "test %s %s ; echo $?" switch
               (rcp-shell-quote-argument (rcp-file-name-path v))))
      (rcp-wait-for-output)
      (goto-char (point-max))
      (forward-line -1)
      (read (current-buffer)))))

(defun rcp-buffer-name (multi-method method user host)
  "A name for the connection buffer for USER at HOST using METHOD."
  (if multi-method
      (rcp-buffer-name-multi-method "rcp" multi-method method user host))
    (format "*rcp/%s %s@%s*" method user host))

(defun rcp-buffer-name-multi-method (prefix multi-method method user host)
  "A name for the multi method connection buffer.
MULTI-METHOD gives the multi method, METHOD the array of methods,
USER the array of user names, HOST the array of host names."
  (unless (and (= (length method) (length user))
               (= (length method) (length host)))
    (error "Syntax error in multi method (implementation error)"))
  (let ((len (length method))
        (i 0)
        string-list)
    (while (< i len)
      (setq string-list (cons (format "%s#%s@%s:"
                                      (aref method i)
                                      (aref user i)
                                      (aref host i))
                              string-list))
      (incf i))
    (format "*%s/%s %s*"
            prefix multi-method
            (apply 'concat (reverse string-list)))))

(defun rcp-get-buffer (multi-method method user host)
  "Get the connection buffer to be used for USER at HOST using METHOD."
  (get-buffer-create (rcp-buffer-name multi-method method user host)))

(defun rcp-debug-buffer-name (multi-method method user host)
  "A name for the debug buffer for USER at HOST using METHOD."
  (if multi-method
      (rcp-buffer-name-multi-method "debug rcp" multi-method method user host)
    (format "*debug rcp/%s %s@%s*" multi-method method user host)))

(defun rcp-get-debug-buffer (multi-method method user host)
  "Get the debug buffer for USER at HOST using METHOD."
  (get-buffer-create (rcp-debug-buffer-name multi-method method user host)))

(defun rcp-find-executable (multi-method method user host progname dirlist)
  "Searches for PROGNAME in all directories mentioned in DIRLIST.
First args METHOD, USER and HOST specify the connection, PROGNAME
is the program to search for, and DIRLIST gives the list of directories
to search.

Returns the full path name of PROGNAME, if found, and nil otherwise.

This function expects to be in the right *rcp* buffer."
  (let (result x)
    (while (and (null result) dirlist)
      (setq x (concat (file-name-as-directory (pop dirlist)) progname))
      (rcp-message 5 "Looking for remote executable `%s'" x)
      (when (rcp-handle-file-executable-p
             (rcp-make-rcp-file-name multi-method method user host x))
        (setq result x)))
    (if result
        (rcp-message 5 "Found remote executable `%s'" result)
      (rcp-message 5 "Couldn't find remote executable `%s'" progname))
    result))

(defun rcp-set-remote-path (multi-method method user host var dirlist)
  "Sets the remote environment VAR to existing directories from DIRLIST.
I.e., for each directory in DIRLIST, it is tested whether it exists and if
so, it is added to the environment variable VAR."
  (let ((existing-dirs
         (mapcar
          (lambda (x)
            (when (and
                   (file-exists-p
                    (rcp-make-rcp-file-name multi-method method user host x))
                   (file-directory-p
                    (rcp-make-rcp-file-name multi-method method user host x)))
              x))
          dirlist)))
    (rcp-send-command
     multi-method method user host
     (concat var "="
             (mapconcat 'identity (delq nil existing-dirs) ":")
             "; export " var))
  (rcp-wait-for-output)))

;; -- communication with external shell --

;; CCC test ksh or bash found for tilde expansion?
(defun rcp-find-shell (multi-method method user host)
  "Find a shell on the remote host which groks tilde expansion."
  (let ((shell nil))
    (rcp-send-command multi-method method user host "echo ~root")
    (rcp-wait-for-output)
    (cond
     ((string-match "^~root$" (buffer-string))
      (setq shell
            (or (rcp-find-executable multi-method method user host
                                     "ksh"  rcp-remote-path)
                (rcp-find-executable multi-method method user host
                                     "bash" rcp-remote-path)))
      (unless shell
        (error "Couldn't find a shell which groks tilde expansion"))
      (rcp-message 5 "Starting remote shell `%s' for tilde expansion..." shell)
      (rcp-send-command multi-method method user host (concat "exec " shell))
      (unless (rcp-wait-for-regexp (get-buffer-process (current-buffer))
                                   60
                                   shell-prompt-pattern)
        (pop-to-buffer (buffer-name))
        (error "Couldn't find remote `%s' prompt."))
      ;(sit-for 1)                       ;why is this needed?
      (process-send-string nil (format "PS1='\n%s\n'; PS2=''; PS3=''\n"
                                       rcp-end-of-output))
      (rcp-send-command multi-method method user host "echo hello")
      (rcp-message 5 "Waiting for remote `%s' to start up..." shell)
      (unless (rcp-wait-for-output 5)
        (unless (rcp-wait-for-output 5)
          (pop-to-buffer (buffer-name))
          (error "Couldn't start remote `%s', see buffer `%s' for details"
                 shell (buffer-name))))
      (rcp-message 5 "Waiting for remote `%s' to start up...done" shell))
     (t (rcp-message 5 "Remote /bin/sh groks tilde expansion, good")))))

(defun rcp-check-ls-command (multi-method method user host cmd)
  "Checks whether the given `ls' executable groks `-n'.
METHOD, USER and HOST specify the connection, CMD (the full path name of)
the `ls' executable.  Returns t if CMD supports the `-n' option, nil
otherwise."
  (rcp-message 9 "Checking remote `%s' command for `-n' option"
               cmd)
  (when (rcp-handle-file-executable-p
         (rcp-make-rcp-file-name multi-method method user host cmd))
    (let ((result nil))
      (rcp-message 7 "Testing remote command `%s' for -n..." cmd)
      (rcp-send-command
       multi-method method user host
       (format "%s -lnd / >/dev/null 2>&1 ; echo $?"
               cmd))
      (rcp-wait-for-output)
      (goto-char (point-min))
      (setq result (zerop (read (current-buffer))))
      (rcp-message 7 "Testing remote command `%s' for -n...%s"
                   cmd
                   (if result "okay" "failed"))
      result)))

(defun rcp-check-ls-commands (multi-method method user host cmd dirlist)
  "Checks whether the given `ls' executable in one of the dirs groks `-n'.
Returns nil if none was found, else the command is returned."
  (let ((dl dirlist)
        (result nil))
    ;; It would be better to use the CL function `find', but
    ;; we don't want run-time dependencies on CL.
    (while (and dl (not result))
      (let ((x (concat (file-name-as-directory (car dl)) cmd)))
        (when (rcp-check-ls-command multi-method method user host x)
          (setq result x)))
      (setq dl (cdr dl)))
    result))

(defun rcp-find-ls-command (multi-method method user host)
  "Finds an `ls' command which groks the `-n' option, returning nil if failed.
\(This option prints numeric user and group ids in a long listing.)"
  (rcp-message 9 "Finding a suitable `ls' command")
  (or
   (rcp-check-ls-commands multi-method method user host "ls" rcp-remote-path)
   (rcp-check-ls-commands multi-method method user host "gnuls" rcp-remote-path)))

;; ------------------------------------------------------------ 
;; -- Functions for establishing connection -- 
;; ------------------------------------------------------------ 

(defun rcp-open-connection-telnet (multi-method method user host)
  "Open a connection using a telnet METHOD.
This starts the command `telnet HOST'[*], then waits for a remote login
prompt, then sends the user name USER, then waits for a remote password
prompt.  It queries the user for the password, then sends the password
to the remote host.

Recognition of the remote shell prompt is based on the variable
`shell-prompt-pattern' which must be set up correctly.

Please note that it is NOT possible to use this connection method
together with an out-of-band transfer method!  You must use an inline
transfer method.

Maybe the different regular expressions need to be tuned.

* Actually, the telnet program to be used can be specified in the
  method parameters, see the variable `rcp-methods'."
  (save-match-data
    (when (rcp-method-out-of-band-p multi-method method)
      (error "Cannot use out-of-band method `%s' with telnet connection method"
             method))
    (when multi-method
      (error "Cannot multi-connect using telnet connection method"))
    (rcp-pre-connection multi-method method user host)
    (rcp-message 7 "Opening connection for %s@%s using %s..." user host method)
    (let* ((default-directory (rcp-temporary-file-directory))
           (p (start-process (rcp-buffer-name multi-method method user host)
                             (rcp-get-buffer multi-method method user host)
                             (rcp-get-telnet-program multi-method method) host))
           (found nil)
           (pw nil))
      (process-kill-without-query p)
      (rcp-message 9 "Waiting for login prompt...")
      ;; CCC adjust regexp here?
      (unless (rcp-wait-for-regexp p nil ".*ogin: *$")
        (pop-to-buffer (buffer-name))
        (error "Couldn't find remote login prompt"))
      (rcp-message 9 "Sending login name %s" user)
      (process-send-string p (concat user "\n"))
      (rcp-message 9 "Waiting for password prompt...")
      ;; CCC adjust regexp here?
      (unless (setq found (rcp-wait-for-regexp p nil ".*assword: *$"))
        (pop-to-buffer (buffer-name))
        (error "Couldn't find remote password prompt"))
      (setq pw (rcp-read-passwd found))
      (rcp-message 9 "Sending password")
      (process-send-string p (concat pw "\n"))
      (rcp-message 9 "Waiting 30s for remote shell to come up...")
      (unless (rcp-wait-for-regexp p 30 (format "\\(%s\\)\\|\\(%s\\)"
                                                shell-prompt-pattern
                                                rcp-wrong-passwd-regexp))
        (pop-to-buffer (buffer-name))
        (error "Couldn't find remote shell prompt"))
      (when (match-string 2)
        (pop-to-buffer (buffer-name))
        (error "Login failed: %s" (match-string 2)))
      (rcp-open-connection-setup-interactive-shell
       p multi-method method user host)
      (rcp-post-connection multi-method method user host))))

(defun rcp-open-connection-rsh (multi-method method user host)
  "Open a connection using an rsh METHOD.
This starts the command `rsh HOST -l USER'[*], then waits for a remote
password or shell prompt.  If a password prompt is seen, the user is
queried for a password, this function sends the password to the remote
host and waits for a shell prompt.

Recognition of the remote shell prompt is based on the variable
`shell-prompt-pattern' which must be set up correctly.

Please note that it is NOT possible to use this connection method with
an inline transfer method if this function asks the user for a
password!  You must use an inline transfer method in this case.
Sadly, the transfer method cannot be switched on the fly, instead you
must specify the right method in the file name.

* Actually, the rsh program to be used can be specified in the
  method parameters, see the variable `rcp-methods'."
  (save-match-data
    (when multi-method
      (error "Cannot multi-connect using rsh connection method"))
    (rcp-pre-connection multi-method method user host)
    (rcp-message 7 "Opening connection for %s@%s using %s..." user host method)
    (let* ((default-directory (rcp-temporary-file-directory))
           (p (apply #'start-process
                     (rcp-buffer-name multi-method method user host)
                     (rcp-get-buffer multi-method method user host)
                     (rcp-get-rsh-program multi-method method) host "-l" user
                     (rcp-get-rsh-args multi-method method)))
           (found nil))
      (process-kill-without-query p)
      (rcp-message 9 "Waiting 60s for shell or passwd prompt from %s" host)
      (setq found
            (rcp-wait-for-regexp
             p 60
             (format
              "\\(%s\\)\\|\\(%s\\)"
              shell-prompt-pattern
              rcp-password-prompt-regexp)))
      (unless found
        (pop-to-buffer (buffer-name))
        (error "Couldn't find remote shell or passwd prompt"))
      (when (match-string 2)
        (when (rcp-method-out-of-band-p multi-method method)
          (pop-to-buffer (buffer-name))
          (error (concat "Out of band method `%s' not applicable"
                         " for remote shell asking for a password")
                 method))
        (rcp-message 9 "Sending password...")
        (rcp-enter-password p (match-string 2))
        (rcp-message 9 "Sent password, waiting 60s for remote shell prompt")
        (setq found (rcp-wait-for-regexp p 60
                                         (format "\\(%s\\)\\|\\(%s\\)"
                                                 shell-prompt-pattern
                                                 rcp-wrong-passwd-regexp))))
      (unless found
        (pop-to-buffer (buffer-name))
        (error "Couldn't find remote shell prompt"))
      (when (match-string 2)
        (pop-to-buffer (buffer-name))
        (error "Login failed: %s" (match-string 2)))
      (rcp-message 7 "Initializing remote shell")
      (rcp-open-connection-setup-interactive-shell
       p multi-method method user host)
      (rcp-post-connection multi-method method user host))))

(defun rcp-open-connection-su (multi-method method user host)
  "Open a connection using the `su' program with METHOD.
This starts `su - USER', then waits for a password prompt.  The HOST
name must be equal to the local host name or to `localhost'.

Recognition of the remote shell prompt is based on the variable
`shell-prompt-pattern' which must be set up correctly.  Note that the
other user may have a different shell prompt than you do, so it is not
at all unlikely that this variable is set up wrongly!"
  (save-match-data
    (when (rcp-method-out-of-band-p multi-method method)
      (error "Cannot use out-of-band method `%s' with `su' connection method"
             method))
    (unless (or (string-match (concat "^" (regexp-quote host))
                              (system-name))
                (string= "localhost" host))
      (error
       "Cannot connect to different host `%s' with `su' connection method"
       host))
    (rcp-pre-connection multi-method method user host)
    (rcp-message 7 "Opening connection for `%s' using `%s'..." user method)
    (let* ((default-directory (rcp-temporary-file-directory))
           (p (apply 'start-process
                     (rcp-buffer-name multi-method method user host)
                     (rcp-get-buffer multi-method method user host)
                     (rcp-get-su-program multi-method method)
                     (append (rcp-get-su-args multi-method method)
                             (list user))))
           (found nil)
           (pw nil))
      (process-kill-without-query p)
      (rcp-message 9 "Waiting 30s for shell or password prompt...")
      ;; CCC adjust regexp here?
      (unless (setq found (rcp-wait-for-regexp
                           p 30
                           (format "\\(%s\\)\\|\\(%s\\)"
                                   shell-prompt-pattern
                                   rcp-password-prompt-regexp)))
        (pop-to-buffer (buffer-name))
        (error "Couldn't find shell or password prompt"))
      (when (match-string 2)
        (setq pw (rcp-read-passwd found))
        (rcp-message 9 "Sending password")
        (process-send-string p (concat pw "\n"))
        (rcp-message 9 "Waiting 30s for remote shell to come up...")
        (unless (rcp-wait-for-regexp p 30 (format "\\(%s\\)\\|\\(%s\\)"
                                                  shell-prompt-pattern
                                                  rcp-wrong-passwd-regexp))
          (pop-to-buffer (buffer-name))
          (error "Couldn't find remote shell prompt"))
        (when (match-string 2)
          (pop-to-buffer (buffer-name))
          (error "`su' failed: %s" (match-string 2))))
      (rcp-open-connection-setup-interactive-shell
       p multi-method method user host)
      (rcp-post-connection multi-method method user host))))

(defun rcp-open-connection-multi (multi-method method user host)
  "Open a multi-hop connection using METHOD.
This uses a slightly changed file name syntax.  The idea is to say
    /r@multi:telnet#u1@h1:rsh#u2@h2:/path/to/file
This will use telnet to log in as u1 to h1, then use rsh from there to
log in as u2 to h2."
  (save-match-data
    (unless multi-method
      (error "Multi-hop open connection function called on non-multi method"))
    (when (rcp-method-out-of-band-p multi-method method)
      (error "No out of band multi-hop connections"))
    (unless (and (arrayp method) (not (stringp method)))
      (error "METHOD must be an array of strings for multi methods"))
    (unless (and (arrayp user) (not (stringp user)))
      (error "USER must be an array of strings for multi methods"))
    (unless (and (arrayp host) (not (stringp host)))
      (error "HOST must be an array of strings for multi methods"))
    (unless (and (= (length method) (length user))
                 (= (length method) (length host)))
      (error "Arrays METHOD, USER, HOST must have equal length"))
    (rcp-pre-connection multi-method method user host)
    (rcp-message 7 "Opening `%s' connection..." multi-method)
    (let* ((default-directory (rcp-temporary-file-directory))
           (p (start-process (rcp-buffer-name multi-method method user host)
                             (rcp-get-buffer multi-method method user host)
                             rcp-sh-program))
           (num-hops (length method))
           (i 0))
      (process-kill-without-query p)
      (rcp-message 9 "Waiting 60s for local shell to come up...")
      (unless (rcp-wait-for-regexp p 60 shell-prompt-pattern)
        (pop-to-buffer (buffer-name))
        (error "Couldn't find local shell prompt"))
      ;; Now do all the connections as specified.
      (while (< i num-hops)
        (let* ((m (aref method i))
               (u (aref user i))
               (h (aref host i))
               (entry (assoc m rcp-multi-connection-function-alist))
               (multi-func (nth 1 entry))
               (program (nth 2 entry)))
          ;; The multi-funcs don't need to do save-match-data, as that
          ;; is done here.
          (funcall multi-func p m u h program)
          (incf i)))
      (erase-buffer)
      (rcp-open-connection-setup-interactive-shell
       p multi-method method user host)
      (rcp-post-connection multi-method method user host))))

(defun rcp-multi-connect-telnet (p method user host program)
  "Issue `telnet' command.
Uses program PROGRAM to issue a `telnet' command to log in as USER to HOST."
  (let (found pw)
    (erase-buffer)
    (rcp-message 9 "Sending telnet command `%s %s'" program host)
    (process-send-string p (format "%s %s\n" program host))
    (rcp-message 9 "Waiting 30s for login prompt from %s" host)
    (unless (rcp-wait-for-regexp p 30 ".*ogin: *$")
      (pop-to-buffer (buffer-name))
      (error "Couldn't find login prompt from host %s" host))
    (rcp-message 9 "Sending login name %s" user)
    (process-send-string p (concat user "\n"))
    (rcp-message 9 "Waiting for password prompt")
    (unless (setq found (rcp-wait-for-regexp p nil ".*assword: *$"))
      (pop-to-buffer (buffer-name))
      (error "Couldn't find password prompt from host %s" host))
    (setq pw (rcp-read-passwd
              (format "Password for %s@%s, %s" user host found)))
    (rcp-message 9 "Sending password")
    (process-send-string p (concat pw "\n"))
    (rcp-message 9 "Waiting 60s for remote shell to come up...")
    (unless (rcp-wait-for-regexp p 60 (format "\\(%s\\)\\|\\(%s\\)"
                                              shell-prompt-pattern
                                              rcp-wrong-passwd-regexp))
      (pop-to-buffer (buffer-name))
      (error "Couldn't find shell prompt from host %s" host))
    (when (match-string 2)
      (pop-to-buffer (buffer-name))
      (error "Login to %s failed: %s" (match-string 2)))))

(defun rcp-multi-connect-rlogin (p method user host program)
  "Issue `rlogin' command.
Uses program PROGRAM to issue an `rlogin' command to log in as USER to HOST."
  (let (found pw)
    (erase-buffer)
    (rcp-message 9 "Sending rlogin command `%s %s -l %s'" program host user)
    (process-send-string p (format "%s %s -l %s\n" program host user))
    (rcp-message 9 "Waiting 60s for shell or passwd prompt from %s" host)
    (unless (setq found
                  (rcp-wait-for-regexp p 60
                                       (format "\\(%s\\)\\|\\(%s\\)"
                                               shell-prompt-pattern
                                               rcp-password-prompt-regexp)))
      (pop-to-buffer (buffer-name))
      (error "Couldn't find remote shell or passwd prompt"))
    (when (match-string 2)
        (rcp-message 9 "Sending password...")
        (rcp-enter-password p (match-string 2))
        (rcp-message 9 "Sent password, waiting 60s for remote shell prompt")
        (setq found (rcp-wait-for-regexp p 60
                                         (format "\\(%s\\)\\|\\(%s\\)"
                                                 shell-prompt-pattern
                                                 rcp-wrong-passwd-regexp))))
      (unless found
        (pop-to-buffer (buffer-name))
        (error "Couldn't find remote shell prompt"))
      (when (match-string 2)
        (pop-to-buffer (buffer-name))
        (error "Login failed: %s" (match-string 2)))))

(defun rcp-multi-connect-su (p method user host program)
  "Issue `su' command.
Uses program PROGRAM to issue a `su' command to log in as USER on
HOST.  The HOST name is ignored, this just changes the user id on the
host currently logged in to."
  (let (found pw)
    (erase-buffer)
    (rcp-message 9 "Sending su command `%s - %s'" program user)
    (process-send-string p (format "%s - %s\n" program user))
    (rcp-message 9 "Waiting 60s for shell or passwd prompt for %s" user)
    (unless (setq found
                  (rcp-wait-for-regexp p 60
                                       (format "\\(%s\\)\\|\\(%s\\)"
                                               shell-prompt-pattern
                                               rcp-password-prompt-regexp)))
      (pop-to-buffer (buffer-name))
      (error "Couldn't find shell or passwd prompt for %s" user))
    (when (match-string 2)
      (rcp-message 9 "Sending password...")
      (rcp-enter-password p (match-string 2))
      (rcp-message 9 "Send password, waiting 60s for remote shell prompt")
      (setq found (rcp-wait-for-regexp p 60
                                       (format "\\(%s\\)\\|\\(%s\\)"
                                               shell-prompt-pattern
                                               rcp-wrong-passwd-regexp))))
    (unless found
      (pop-to-buffer (buffer-name))
      (error "Couldn't find remote shell prompt"))
    (when (match-string 2)
      (pop-to-buffer (buffer-name))
      (error "Login failed: %s" (match-string 2)))))

;; Utility functions.

(defun rcp-wait-for-regexp (proc timeout regexp)
  "Wait for a REGEXP to appear from process PROC within TIMEOUT seconds.
Expects the output of PROC to be sent to the current buffer.  Returns
the string that matched, or nil.  Waits indefinitely if TIMEOUT is
nil."
  (let ((found nil))
    (cond (timeout
           (with-timeout (timeout)
             (while (not found)
               (accept-process-output proc 1)
               (goto-char (point-min))
               (setq found (when (re-search-forward regexp nil t)
                             (match-string 0))))))
          (t
           (while (not found)
             (accept-process-output proc 1)
             (goto-char (point-min))
             (setq found (when (re-search-forward regexp nil t)
                           (match-string 0))))))
    found))

(defun rcp-enter-password (p prompt)
  "Prompt for a password and send it to the remote end.
Uses PROMPT as a prompt and sends the password to process P."
  (let ((pw (rcp-read-passwd prompt)))
    (process-send-string p (concat pw rcp-rsh-end-of-line))))

(defun rcp-pre-connection (multi-method method user host)
  "Do some setup before actually logging in.
METHOD, USER and HOST specify the connection."
  (set-buffer (rcp-get-buffer multi-method method user host))
  (set (make-local-variable 'rcp-current-multi-method) multi-method)
  (set (make-local-variable 'rcp-current-method) method)
  (set (make-local-variable 'rcp-current-user)   user)
  (set (make-local-variable 'rcp-current-host)   host)
  (erase-buffer))

(defun rcp-open-connection-setup-interactive-shell
  (p multi-method method user host)
  "Set up an interactive shell.
Mainly sets the prompt and the echo correctly.  P is the shell process
to set up.  METHOD, USER and HOST specify the connection."
  (process-send-string nil "exec /bin/sh\n")
  (rcp-message 9 "Waiting 30s for remote /bin/sh to come up...")
  (unless (rcp-wait-for-regexp p 30
                               (format "\\(\\$\\|%s\\)" shell-prompt-pattern))
    (pop-to-buffer (buffer-name))
    (error "Remote /bin/sh didn't come up.  See buffer `%s' for details"
           (buffer-name)))
  (rcp-message 9 "Setting up remote shell environment")
  (rcp-send-command
   multi-method method user host
   (format (concat "stty -onlcr -echo 1>/dev/null 2>/dev/null ; "
                   "unset MAIL ; set +o history 1>/dev/null 2>/dev/null ; "
                   "PS1='\n%s\n'; PS2=''; PS3=''\n")
           rcp-end-of-output))
  (unless (rcp-wait-for-output 5)
    (pop-to-buffer (buffer-name))
    (error "Couldn't set remote shell prompt."))
  (rcp-message 9 "Waiting for remote /bin/sh to come up...")
  (unless (rcp-wait-for-output 5)
    (unless (rcp-wait-for-output 5)
      (pop-to-buffer (buffer-name))
      (error "Remote /bin/sh didn't come up.  See buffer `%s' for details"
             (buffer-name))))
  (rcp-message 7 "Waiting for remote /bin/sh to come up...done"))

(defun rcp-post-connection (multi-method method user host)
  "Prepare a remote shell before being able to work on it.
METHOD, USER and HOST specify the connection.
Among other things, this finds a shell which groks tilde expansion,
tries to find an `ls' command which groks the `-n' option, sets the
locale to C and sets up the remote shell search path."
  (rcp-find-shell multi-method method user host)
  (sit-for 1)
  ;; Without (sit-for 0.1) at least, my machine will almost always blow
  ;; up on 'not numberp /root' - a race that causes the 'echo ~root'
  ;; output of (rcp-find-shell) to show up along with the output of
  ;; (rcp-find-ls-command) testing.
  ;;
  ;; I can't work out why this is a problem though. The (rcp-wait-for-output)
  ;; call in (rcp-find-shell) *should* make this not happen, I thought.
  ;;
  ;; After much debugging I couldn't find any problem with the implementation
  ;; of that function though. The workaround stays for me at least. :/
  ;;
  ;; Daniel Pittman <daniel@danann.net>
  (make-local-variable 'rcp-ls-command)
  (setq rcp-ls-command (rcp-find-ls-command multi-method method user host))
  (unless rcp-ls-command
    (rcp-message
     1
     "Danger!  Couldn't find ls which groks -n.  Muddling through anyway")
    (setq rcp-ls-command
          (rcp-find-executable multi-method method user host
                               "ls" rcp-remote-path)))
  (unless rcp-ls-command
    (error "Fatal error: Couldn't find remote executable `ls'"))
  (rcp-message 5 "Using remote command `%s' for getting directory listings"
               rcp-ls-command)
  ;; Tell remote shell to use standard time format, needed for
  ;; parsing `ls -l' output.
  (rcp-send-command multi-method method user host
                    (concat "rcp_set_exit_status () {\n"
                            "return $1\n"
                            "}"))
  (rcp-wait-for-output)
  ;; Set remote PATH variable.
  (rcp-set-remote-path multi-method method user host "PATH" rcp-remote-path)
  (rcp-send-command multi-method method user host
                    "LC_TIME=C; export LC_TIME; echo huhu")
  (rcp-wait-for-output))

(defun rcp-maybe-open-connection (multi-method method user host)
  "Maybe open a connection to HOST, logging in as USER, using METHOD.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  (let ((p (get-buffer-process (rcp-get-buffer multi-method method user host))))
    (unless (and p
                 (processp p)
                 (memq (process-status p) '(run open)))
      (when (and p (processp p))
        (delete-process p))
      (funcall (rcp-get-connection-function multi-method method)
               multi-method method user host))))

(defun rcp-send-command
  (multi-method method user host command &optional noerase)
  "Send the COMMAND to USER at HOST (logged in using METHOD).
Erases temporary buffer before sending the command (unless NOERASE
is true)."
  (rcp-maybe-open-connection multi-method method user host)
  (when rcp-debug-buffer
    (save-excursion
      (set-buffer (rcp-get-debug-buffer multi-method method user host))
      (goto-char (point-max))
      (insert "$ " command "\n")))
  (let ((proc nil))
    (set-buffer (rcp-get-buffer multi-method method user host))
    (unless noerase (erase-buffer))
    (setq proc (get-buffer-process (current-buffer)))
    (process-send-string proc
                         (concat command rcp-rsh-end-of-line))))

(defun rcp-wait-for-output (&optional timeout)
  "Wait for output from remote rsh command."
  (let ((proc (get-buffer-process (current-buffer)))
        (result nil)
        (found nil)
        (end-of-output (concat "^"
                               (regexp-quote rcp-end-of-output)
                               "$")))
    ;; Algorithm: get waiting output.  See if last line contains
    ;; end-of-output sentinel.  If not, wait a bit and again get
    ;; waiting output.  Repeat until timeout expires or end-of-output
    ;; sentinel is seen.  Will hang if timeout is nil and
    ;; end-of-output sentinel never appears.
    (save-match-data
      (cond (timeout
             (with-timeout (timeout)
               (while (not found)
                 (accept-process-output proc 1)
                 (goto-char (point-max))
                 (forward-line -1)
                 (setq found (looking-at end-of-output)))))
            (t
             (while (not found)
               (accept-process-output proc 1)
               (goto-char (point-max))
               (forward-line -1)
               (setq found (looking-at end-of-output))))))
    ;; At this point, either the timeout has expired or we have found
    ;; the end-of-output sentinel.
    (when found
      (goto-char (point-max))
      (forward-line -2)
      (delete-region (point) (point-max)))
    ;; Add output to debug buffer if appropriate.
    (when rcp-debug-buffer
      (append-to-buffer
       (rcp-get-debug-buffer rcp-current-multi-method rcp-current-method
                             rcp-current-user rcp-current-host)
       (point-min) (point-max))
      (when (not found)
        (save-excursion
          (set-buffer
           (rcp-get-debug-buffer rcp-current-multi-method rcp-current-method
                                 rcp-current-user rcp-current-host))
          (goto-char (point-max))
          (insert "[[INCOMPLETE!]]"))))
    (goto-char (point-min))
    ;; Return value is whether end-of-output sentinel was found.
    found))

(defun rcp-barf-unless-okay (fmt &rest args)
  "Expects same arguments as `error'.  Checks if previous command was okay.
Requires that previous command includes `echo $?'."
  (rcp-wait-for-output)
  (goto-char (point-max))
  (forward-line -1)
  (let ((x (read (current-buffer))))
    (unless (and x (numberp x) (zerop x))
      (pop-to-buffer (current-buffer))
      (apply 'error fmt args))))

(defun rcp-send-region (multi-method method user host start end)
  "Send the region from START to END to remote command
running as USER on HOST using METHOD."
  (let ((proc (get-buffer-process
               (rcp-get-buffer multi-method method user host))))
    (unless proc
      (error "Can't send region to remote host -- not logged in"))
    (process-send-region proc start end)
    (when rcp-debug-buffer
      (append-to-buffer
       (rcp-get-debug-buffer multi-method method user host)
       start end))))

(defun rcp-send-eof (multi-method method user host)
  "Send EOF to the remote end.
METHOD, HOST and USER specify the the connection."
  (let ((proc (get-buffer-process
               (rcp-get-buffer multi-method method user host))))
    (unless proc
      (error "Can't send EOF to remote host -- not logged in"))
    (process-send-eof proc)))

(defun rcp-mode-string-to-int (mode-string)
  "Converts a ten-letter `drwxrwxrwx'-style mode string into mode bits."
  (let* ((mode-chars (string-to-vector mode-string))
         (owner-read (aref mode-chars 1))
         (owner-write (aref mode-chars 2))
         (owner-execute-or-setid (aref mode-chars 3))
         (group-read (aref mode-chars 4))
         (group-write (aref mode-chars 5))
         (group-execute-or-setid (aref mode-chars 6))
         (other-read (aref mode-chars 7))
         (other-write (aref mode-chars 8))
         (other-execute-or-sticky (aref mode-chars 9)))
    (save-match-data
      (logior
       (case owner-read
         (?r (rcp-octal-to-decimal "00400")) (?- 0)
         (t (error "Second char `%c' must be one of `r-'" owner-read)))
       (case owner-write
         (?w (rcp-octal-to-decimal "00200")) (?- 0)
         (t (error "Third char `%c' must be one of `w-'" owner-write)))
       (case owner-execute-or-setid
         (?x (rcp-octal-to-decimal "00100"))
         (?S (rcp-octal-to-decimal "04000"))
         (?s (rcp-octal-to-decimal "04100"))
         (?- 0)
         (t (error "Fourth char `%c' must be one of `xsS-'"
                   owner-execute-or-setid)))
       (case group-read
         (?r (rcp-octal-to-decimal "00040")) (?- 0)
         (t (error "Fifth char `%c' must be one of `r-'" group-read)))
       (case group-write
         (?w (rcp-octal-to-decimal "00020")) (?- 0)
         (t (error "Sixth char `%c' must be one of `w-'" group-write)))
       (case group-execute-or-setid
         (?x (rcp-octal-to-decimal "00010"))
         (?S (rcp-octal-to-decimal "02000"))
         (?s (rcp-octal-to-decimal "02010"))
         (?- 0)
         (t (error "Seventh char `%c' must be one of `xsS-'"
                   group-execute-or-setid)))
       (case other-read
         (?r (rcp-octal-to-decimal "00004")) (?- 0)
         (t (error "Eighth char `%c' must be one of `r-'" other-read)))
       (case other-write
         (?w (rcp-octal-to-decimal "00002")) (?- 0)
         (t (error "Nineth char `%c' must be one of `w-'" other-write)))
       (case other-execute-or-sticky
         (?x (rcp-octal-to-decimal "00001"))
         (?T (rcp-octal-to-decimal "01000"))
         (?t (rcp-octal-to-decimal "01001"))
         (?- 0)
         (t (error "Tenth char `%c' must be one of `xtT-'"
                   other-execute-or-sticky)))))))

(defun rcp-decimal-to-octal (i)
  "Return a string consisting of the octal digits of I.
Not actually used.  Use `(format \"%o\" i)' instead?"
  (cond ((< i 0) (error "Cannot convert negative number to octal"))
        ((not (integerp i)) (error "Cannot convert non-integer to octal"))
        ((zerop i) "0")
        (t (concat (rcp-decimal-to-octal (/ i 8))
                   (number-to-string (% i 8))))))

;;(defun rcp-octal-to-decimal (ostr)
;;  "Given a string of octal digits, return a decimal number."
;;  (cond ((null ostr) 0)
;;        ((string= "" ostr) 0)
;;        (t (let ((last (aref ostr (1- (length ostr))))
;;                 (rest (substring ostr 0 (1- (length ostr)))))
;;             (unless (and (>= last ?0)
;;                          (<= last ?7))
;;               (error "Not an octal digit: %c" last))
;;             (+ (- last ?0) (* 8 (rcp-octal-to-decimal rest)))))))
;; Kudos to Gerd Moellmann for this suggestion.
(defun rcp-octal-to-decimal (ostr)
  "Given a string of octal digits, return a decimal number."
  (let ((x (or ostr "")))
    ;; `save-match' is in `rcp-mode-string-to-int' which calls this.
    (unless (string-match "\\`[0-7]*\\'" ostr)
      (error "Non-octal junk in string `%s'" ostr))
    (string-to-number ostr 8)))

;; ------------------------------------------------------------ 
;; -- RCP file names -- 
;; ------------------------------------------------------------ 
;; Conversion functions between external representation and
;; internal data structure.  Convenience functions for internal
;; data structure.

(defstruct rcp-file-name multi-method method user host path)

(defun rcp-rcp-file-p (name)
  "Return t iff NAME is an rcp file."
  (save-match-data
    (string-match rcp-file-name-regexp name)))

(defun rcp-dissect-file-name (name)
  "Return an `rcp-file-name' structure.
The structure consists of remote method, remote user, remote host and
remote path name."
  (let (method)
    (save-match-data
      (unless (string-match (nth 0 rcp-file-name-structure) name)
        (error "Not an rcp file name: %s" name))
      (setq method (or (match-string (nth 1 rcp-file-name-structure) name)
                       rcp-default-method))
      (if (member method rcp-multi-methods)
          ;; If it's a multi method, the file name structure contains
          ;; arrays of method, user and host.
          (rcp-dissect-multi-file-name name)
        ;; Normal method.
        (make-rcp-file-name
         :multi-method nil
         :method method
         :user (or (match-string (nth 2 rcp-file-name-structure) name)
                   (user-login-name))
         :host (match-string (nth 3 rcp-file-name-structure) name)
         :path (match-string (nth 4 rcp-file-name-structure) name))))))

(defun rcp-dissect-multi-file-name (name)
  "Not implemented yet."
  (let ((regexp           (nth 0 rcp-multi-file-name-structure))
        (method-index     (nth 1 rcp-multi-file-name-structure))
        (hops-index       (nth 2 rcp-multi-file-name-structure))
        (path-index       (nth 3 rcp-multi-file-name-structure))
        (hop-regexp       (nth 0 rcp-multi-file-name-hop-structure))
        (hop-method-index (nth 1 rcp-multi-file-name-hop-structure))
        (hop-user-index   (nth 2 rcp-multi-file-name-hop-structure))
        (hop-host-index   (nth 3 rcp-multi-file-name-hop-structure))
        method hops len hop-methods hop-users hop-hosts path)
    (unless (string-match (format regexp hop-regexp) name)
      (error "Not a multi rcp file name: %s" name))
    (setq method (match-string method-index name))
    (setq hops (match-string hops-index name))
    (setq len (/ (length (match-data t)) 2))
    (when (< path-index 0) (incf path-index len))
    (setq path (match-string path-index name))
    (let ((index 0))
      (while (string-match hop-regexp hops index)
        (setq index (match-end 0))
        (setq hop-methods
              (cons (match-string hop-method-index hops) hop-methods))
        (setq hop-users
              (cons (match-string hop-user-index hops) hop-users))
        (setq hop-hosts
              (cons (match-string hop-host-index hops) hop-hosts))))
    (make-rcp-file-name
     :multi-method method
     :method       (apply 'vector (reverse hop-methods))
     :user         (apply 'vector (reverse hop-users))
     :host         (apply 'vector (reverse hop-hosts))
     :path         path)))

(defun rcp-make-rcp-file-name (multi-method method user host path)
  "Constructs an rcp file name from METHOD, USER, HOST and PATH."
  (unless rcp-make-rcp-file-format
    (error "`rcp-make-rcp-file-format' is nil"))
  (if multi-method
      (rcp-make-rcp-multi-file-name multi-method method user host path)
    (rcp-substitute-percent-escapes rcp-make-rcp-file-format
                                    (list (cons "%%" "%")
                                          (cons "%m" method)
                                          (cons "%u" user)
                                          (cons "%h" host)
                                          (cons "%p" path)))))

(defun rcp-make-rcp-multi-file-name (multi-method method user host path)
  "Constructs an rcp file name for a multi-hop method."
  (unless rcp-make-multi-rcp-file-format
    (error "`rcp-make-multi-rcp-file-format' is nil"))
  (let* ((prefix-format (nth 0 rcp-make-multi-rcp-file-format))
         (hop-format    (nth 1 rcp-make-multi-rcp-file-format))
         (path-format   (nth 2 rcp-make-multi-rcp-file-format))
         (prefix (rcp-substitute-percent-escapes
                  prefix-format
                  (list (cons "%%" "%")
                        (cons "%m" multi-method))))
         (hops "")
         (path (rcp-substitute-percent-escapes
                path-format
                (list (cons "%%" "%")
                      (cons "%p" path))))
         (i 0)
         (len (length method)))
    (while (< i len)
      (let ((m (aref method i))
            (u (aref user i))
            (h (aref host i)))
        (setq hops (concat hops
                           (rcp-substitute-percent-escapes
                            hop-format
                            (list (cons "%%" "%")
                                  (cons "%m" m)
                                  (cons "%u" u)
                                  (cons "%h" h)))))
        (incf i)))
    (concat prefix hops path)))

(defun rcp-make-rcp-program-file-name (user host path)
  "Create a file name suitable to be passed to `rcp'."
  (format "%s@%s:%s" user host path))

(defun rcp-method-out-of-band-p (multi-method method)
  "Return t if this is an out-of-band method, nil otherwise.
It is important to check for this condition, since it is not possible
to enter a password for the `rcp-rcp-program'."
  (rcp-get-rcp-program multi-method method))

;; Variables local to connection.

(defun rcp-get-ls-command (multi-method method user host)
  (save-excursion
    (rcp-maybe-open-connection multi-method method user host)
    (set-buffer (rcp-get-buffer multi-method method user host))
    rcp-ls-command))

(defun rcp-get-connection-function (multi-method method)
  (second (or (assoc 'rcp-connection-function
                     (assoc (or multi-method method rcp-default-method)
                            rcp-methods))
              (error "Method `%s' didn't specify a connection function"
                     (or multi-method method)))))

(defun rcp-get-rsh-program (multi-method method)
  (second (or (assoc 'rcp-rsh-program
                     (assoc (or multi-method method rcp-default-method)
                            rcp-methods))
              (error "Method `%s' didn't specify an rsh program"
                     (or multi-method method)))))

(defun rcp-get-rsh-args (multi-method method)
  (second (or (assoc 'rcp-rsh-args
                     (assoc (or multi-method method rcp-default-method)
                            rcp-methods))
              (error "Method `%s' didn't specify rsh args"
                     (or multi-method method)))))

(defun rcp-get-rcp-program (multi-method method)
  (second (or (assoc 'rcp-rcp-program
                     (assoc (or multi-method method rcp-default-method)
                            rcp-methods))
              (error "Method `%s' didn't specify an rcp program"
                     (or multi-method method)))))

(defun rcp-get-rcp-args (multi-method method)
  (second (or (assoc 'rcp-rcp-args
                     (assoc (or multi-method method rcp-default-method)
                            rcp-methods))
              (error "Method `%s' didn't specify rcp args"
                     (or multi-method method)))))

(defun rcp-get-rcp-keep-date-arg (multi-method method)
  (second (or (assoc 'rcp-rcp-keep-date-arg
                     (assoc (or multi-method method rcp-default-method)
                            rcp-methods))
              (error "Method `%s' didn't specify `keep-date' arg for rcp"
                     (or multi-method method)))))

(defun rcp-get-su-program (multi-method method)
  (second (or (assoc 'rcp-su-program
                     (assoc (or multi-method method rcp-default-method)
                            rcp-methods))
              (error "Method `%s' didn't specify a su program"
                     (or multi-method method)))))

(defun rcp-get-su-args (multi-method method)
  (second (or (assoc 'rcp-su-args
                     (assoc (or multi-method method rcp-default-method)
                            rcp-methods))
              (error "Method `%s' didn't specify su args"
                     (or multi-method method)))))

(defun rcp-get-encoding-command (multi-method method)
  (second (or (assoc 'rcp-encoding-command
                     (assoc (or multi-method method rcp-default-method)
                            rcp-methods))
              (error "Method `%s' didn't specify an encoding command"
                     (or multi-method method)))))

(defun rcp-get-decoding-command (multi-method method)
  (second (or (assoc 'rcp-decoding-command
                     (assoc (or multi-method method rcp-default-method)
                            rcp-methods))
              (error "Method `%s' didn't specify a decoding command"
                     (or multi-method method)))))

(defun rcp-get-encoding-function (multi-method method)
  (second (or (assoc 'rcp-encoding-function
                     (assoc (or multi-method method rcp-default-method)
                            rcp-methods))
              (error "Method `%s' didn't specify an encoding function"
                     (or multi-method method)))))

(defun rcp-get-decoding-function (multi-method method)
  (second (or (assoc 'rcp-decoding-function
                     (assoc (or multi-method method rcp-default-method)
                            rcp-methods))
              (error "Method `%s' didn't specify a decoding function"
                     (or multi-method method)))))

(defun rcp-get-telnet-program (multi-method method)
  (second (or (assoc 'rcp-telnet-program
                     (assoc (or multi-method method rcp-default-method)
                            rcp-methods))
              (error "Method `%s' didn't specify a telnet program"
                     (or multi-method method)))))

;; general utility functions

;; This definition commented out.  Maybe the next one is faster?
;;-(defun rcp-substitute-percent-escapes (str escapes)
;;-  "Given a STRing, does percent substitution according to ESCAPES.
;;-ESCAPES is an alist where the keys are strings of the form \"%x\" and the
;;-values are replacement strings.  In STR, all occurrences of \"%x\" are
;;-replaced with the given replacement string."
;;-  (let ((m (string-match "\\([^%]*\\)\\(%.\\)\\(.*\\)" str))
;;-        a b c)
;;-    (if (not m)
;;-        ;; no percent escape in string, return string unchanged
;;-        str
;;-      ;; first percent escape found, replace it
;;-      (setq a (match-string 1 str))     ;left part
;;-      (setq b (match-string 2 str))     ;middle part -- first percent escape
;;-      (setq c (match-string 3 str))     ;right part
;;-      ;; return value is left part plus replaced middle part
;;-      ;; plus replacement of right part
;;-      (concat a
;;-              (cdr (or (assoc b escapes)
;;-                       (error "Unknown format code: %s" b)))
;;-              (rcp-substitute-percent-escapes c escapes)))))

;; Maybe this definition is faster than regexp matching?
(defun rcp-substitute-percent-escapes (str escapes)
  (save-excursion
    (let ((buf (get-buffer-create " *rcp replace*"))
          code e)
      (set-buffer buf)
      (erase-buffer)
      (insert str)
      (insert "\n")                     ;extra char because of eobp
      (goto-char (point-min))
      (skip-chars-forward "^%")
      (condition-case err (forward-char 2) (end-of-buffer nil))
      (while (not (eobp))
        (setq e (point))
        (backward-char 2)
        (setq code (buffer-substring (point) e))
        (delete-char 2)
        (let ((x (assoc code escapes)))
          (unless x (message "Unknown format code: `%s'" code))
          (when (cdr x) (insert (cdr x))))
        (skip-chars-forward "^%")
        (condition-case err (forward-char 2) (end-of-buffer nil)))
      (goto-char (point-max))
      (backward-char 1)
      (delete-char 1)
      (buffer-string))))

;; Auto saving to a special directory.

(defun rcp-make-auto-save-file-name (fn)
  "Returns a file name in `rcp-auto-save-directory' for autosaving this file."
  (when rcp-auto-save-directory
    (unless (file-exists-p rcp-auto-save-directory)
      (make-directory rcp-auto-save-directory t)))
  (expand-file-name
   (rcp-subst-strs-in-string '(("_" . "|")
                               ("/" . "_a")
                               (":" . "_b")
                               ("|" . "__"))
                             fn)
   rcp-auto-save-directory))

(defadvice make-auto-save-file-name
  (around rcp-advice-make-auto-save-file-name () activate)
  "Invoke `rcp-make-auto-save-file-name' for rcp files."
  (if (and (buffer-file-name) (rcp-rcp-file-p (buffer-file-name)))
      (setq ad-return-value
            (rcp-make-auto-save-file-name (buffer-file-name)))
    ad-do-it))

(defun rcp-subst-strs-in-string (alist string)
  "Replace all occurrences of the string FROM with TO in STRING.
ALIST is of the form ((FROM . TO) ...)."
  (save-match-data
    (while alist
      (let* ((pr (car alist))
             (from (car pr))
             (to (cdr pr)))
        (while (string-match (regexp-quote from) string)
          (setq string (replace-match to t t string)))
        (setq alist (cdr alist))))
    string))

;; ------------------------------------------------------------
;; -- Compatibility functions section --
;; ------------------------------------------------------------

(defun rcp-temporary-file-directory ()
  "Return name of directory for temporary files (compat function).
For Emacs, this is the variable `temporary-file-directory', for XEmacs
this is the function `temp-directory'."
  (cond ((boundp 'temporary-file-directory) temporary-file-directory)
        ((fboundp 'temp-directory)
         (funcall (symbol-function 'temp-directory)))
        (t (message (concat "Neither `temporary-file-directory' nor "
                            "`temp-directory' is defined -- using /tmp."))
           (file-name-as-directory "/tmp"))))

(defun rcp-read-passwd (prompt)
  "Read a password from user (compat function).
Invokes `read-passwd' if that is defined, else `ange-ftp-read-passwd'."
  (apply
   (if (fboundp 'read-passwd) #'read-passwd #'ange-ftp-read-passwd)
   (list prompt)))

;; Currently (as of Emacs 20.5), the function `shell-quote-argument'
;; does not deal well with newline characters.  Newline is replaced by
;; backslash newline.  But if, say, the string `a backslash newline b'
;; is passed to a shell, the shell will expand this into "ab",
;; completely omitting the newline.  This is not what was intended.
;; It does not appear to be possible to make the function
;; `shell-quote-argument' work with newlines without making it
;; dependent on the shell used.  But within this package, we know that
;; we will always use a Bourne-like shell, so we use an approach which
;; groks newlines.
;;
;; The approach is simple: we call `shell-quote-argument', then
;; massage the newline part of the result.
;;
;; Thanks to Mario DeWeerd for the hint that it is sufficient for this
;; function to work with Bourne-like shells.
(defun rcp-shell-quote-argument (s)
  "Similar to `shell-quote-argument', but groks newlines.
Only works for Bourne-like shells."
  (save-match-data
    (let ((result (shell-quote-argument s))
          (nl (regexp-quote "\\\n")))
      (while (string-match nl result)
        (setq result (replace-match "'\n'" t t result)))
      result)))

;; EFS hooks itself into the file name handling stuff in more places
;; than just `file-name-handler-alist'. The following tells EFS to stay
;; away from rcp.el paths.
;;
;; This is needed because EFS installs (efs-dired-before-readin) into
;; 'dired-before-readin-hook'. This prevents EFS from opening an FTP
;; connection to help it's dired process. Not that I have any real
;; idea *why* this is helpful to dired.
;;
;; Anyway, this advice fixes the problem (with a sledgehammer :)
;;
;; Daniel Pittman <daniel@danann.net>
(defadvice efs-ftp-path (around dont-match-rcp-path activate protect)
  "Cause efs-ftp-path to fail when the path is an RCP path."
  (if (rcp-rcp-file-p (ad-get-arg 0))
      nil
    ad-do-it))


;; Make the `reporter` functionality available for making bug reports about
;; the package. A most useful piece of code.
(defun rcp-bug ()
  "Submit a bug report to the RCP developers."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p	t))
    (reporter-submit-bug-report
     rcp-bug-report-address		; to-address
     (format "rcp (%s)" rcp-version)	; package name and version
     '(;; Current state
       rcp-ls-command
       rcp-currrent-multi-method
       rcp-current-method
       rcp-current-user
       rcp-current-host

       ;; System defaults
       rcp-auto-save-directory		; vars to dump
       rcp-default-method
       rcp-rsh-end-of-line
       rcp-remote-path
       rcp-password-prompt-regexp
       rcp-wrong-passwd-regexp
       rcp-temp-name-prefix
       rcp-file-name-structure
       rcp-file-name-regexp
       rcp-make-rcp-file-format
       rcp-end-of-output

       ;; Non-rcp variables of interest
       shell-prompt-pattern)
     nil				; pre-hook
     nil				; post-hook
     "Enter your bug report in this message, including as much detail as you
possibly can about the problem, what you did to cause it and what the local
and remote machines are.

If you can give a simple set of instructions to make this bug happen reliably,
please include those.  Thank you for helping kill bugs in RCP.")))

;;; TODO:

;; * `vc-directory' does not work.  It never displays any files, even
;;   if it does show files when run locally.
;; * Should we make the shell setup stuff smarter?  For example,
;;   we could try to intercept prompts from the `tset' program
;;   and enter `dumb' as terminal type.
;;   "Edward J. Sabol" <sabol@alderaan.gsfc.nasa.gov>
;; * Bug with file name completion if `@user' part is omitted.
;; * Unify rcp-handle-file-attributes and rcp-file-owner.
;; * Greg Stark: save a read-only file, Emacs asks whether to save
;;   anyway, then tries to chmod the file, which fails.
;; * Make sure permissions of tmp file are good.
;;   (Nelson Minar <nelson@media.mit.edu>)
;; * rcp program name should be customizable on per-host basis?
;;   (Francesco PotortÅÏ <F.Potorti@cnuce.cnr.it>)
;; * Grok passwd prompts with scp?  (David Winter
;;   <winter@nevis1.nevis.columbia.edu>).  Maybe just do `ssh -l user
;;   host', then wait a while for the passwd or passphrase prompt.  If
;;   there is one, remember the passwd/phrase.
;; * Find out atime, mtime and ctime of remote file?
;; * Is the dummy `file-truename' function we've got really sufficient?
;;   NO! -- see 'realpath(3)' for what we need to do. -- <daniel@danann.net>
;; * How to deal with MULE in `insert-file-contents' and `write-region'?
;; * Do asynchronous `shell-command's.
;; * Grok `append' and `lockname' parameters for `write-region'.
;; * Test remote ksh or bash for tilde expansion in `rcp-find-shell'?
;; * abbreviate-file-name
;; * file name completion doesn't work for /r:user@host:<TAB>?
;;   (Henrik Holm <henrikh@tele.ntnu.no>)
;; * grok ~ in rcp-remote-path  (Henrik Holm <henrikh@tele.ntnu.no>)
;; * `C' in dired gives error `not rcp file name'.
;; * instead of putting in user-login-name as remote login, rely
;;   on ssh/scp to fill these in.  Make this controllable with a variable.
;;   I would prefer to use nothing if nothing was specified -- <daniel@danann.net>
;; * Also allow to omit user names when doing multi-hop.  Not sure yet
;;   what the user names should default to, though.
;; * better error checking.  At least whenever we see something
;;   strange when doing zerop, we should kill the process and start
;;   again.  (Greg Stark)
;; * Add caching for filename completion.  (Greg Stark)
;;   Of course, this has issues with usability (stale cache bites) 
;;      -- <daniel@danann.net>
;; * Provide a local cache of old versions of remote files for the rsync
;;   transfer method to use.  (Greg Stark)
;; * Remove unneeded parameters from methods.
;; * Invoke rsync once for copying a whole directory hierarchy.
;;   (Francesco PotortÅÏ)
;; * Should we set PATH ourselves or should we rely on the remote end
;;   to do it?


;; Functions for file-name-handler-alist:
;; diff-latest-backup-file -- in diff.el
;; directory-file-name -- use primitive?
;; dired-compress-file
;; dired-uncache -- this will be needed when we do insert-directory caching
;; file-name-as-directory -- use primitive?
;; file-name-directory -- use primitive?
;; file-name-nondirectory -- use primitive?
;; file-name-sans-versions -- use primitive?
;; file-newer-than-file-p
;; file-ownership-preserved-p
;; find-backup-file-name
;; get-file-buffer -- use primitive
;; load
;; make-symbolic-link
;; set-file-modes
;; set-visited-file-modtime
;; shell-command
;; unhandled-file-name-directory
;; vc-registered
;; verify-visited-file-modtime

;; PERFORMANCE!!!
;;
;; The following functions seem excessively slow with my light usage for this
;; evening. These numbers are from ELP on XEmacs, not byte-complied.

;; Function Name                   Call Count  Elapsed Time  Average Time
;; ==============================  ==========  ============  ============
;; rcp-handle-expand-file-name     2874        36.806337999  0.0128066590
;; rcp-substitute-percent-escapes  8970        14.088506999  0.0015706250
;; rcp-make-rcp-file-name          1794        6.0438420001  0.0033689197
;; rcp-dissect-file-name           2376        4.9513150000  0.0020838867

;; I think that I will go through and remove the redundant calls to the
;; filename mangling functions some time real soon now...

(provide 'rcp)

;;; rcp.el ends here
