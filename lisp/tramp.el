;;; rcp.el --- remote file editing using rsh/rcp or work-alike programs

;; Copyright (C) 1998, 1999 Free Software Foundation, Inc.

;; Author: Kai.Grossjohann@CS.Uni-Dortmund.DE
;; Keywords: comm, processes
;; Version: $Id: tramp.el,v 1.146 1999/09/21 13:26:53 grossjoh Exp $

;; rcp.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; rcp.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
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
;; system without having to enter a password.  Some of the inline
;; methods also require this, but those methods which use the function
;; `rcp-open-connection-telnet' or `rcp-open-connection-rlogin' ask
;; you for a password and wait for a password prompt from the remote
;; host.
;;
;; This package has received some testing, but there is little error
;; recovery code.  That is, if something unexpected happens, this
;; package will bug out with a potentially very cryptic error message.
;; Please help me improve this package by telling me about these
;; unusual situations.
;;
;; Known problems:
;;   - BSD ls doesn't grok `-n' option for printing numeric user and
;;     group ids.  Use `gnuls' instead.
;;   - You can't use rcp together with EFS in XEmacs.  You must do the
;;     following to use rcp.el with XEmacs:
;;         (setq file-name-handler-alist nil)
;;         (require 'rcp)
;;
;; Also see the todo list at the bottom of this file.
;;
;; The current version of rcp.el can be retrieved from the following
;; URL:  ftp://ls6-ftp.cs.uni-dortmund.de/pub/src/emacs/rcp.el
;;
;; There's a mailing list for this, as well.  Its name is:
;;                emacs-rcp@ls6.cs.uni-dortmund.de
;; Send a mail with `help' in the subject (!) to the administration
;; address for instructions on joining the list.  The administration
;; address is:
;;            emacs-rcp-request@ls6.cs.uni-dortmund.de
;; You may also mail me, Kai, directly.

;;; Code:

;; CCC: The following require should be removed once the integration
;; with VC is clear.  Talking to Andre Spiegel about this.
(require 'vc)                           ;for doing remote vc

;; Can't be in eval-when-compile because of `defstruct'.
(require 'cl)

(eval-when-compile
  (require 'custom)
  ;; Emacs 19.34 compatibility hack -- is this needed?
  (or (>= emacs-major-version 20)
      (load "cl-seq")))

(provide 'rcp)

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

;; CCC I have changed all occurrences of comint-quote-filename with
;; shell-quote-argument, except in rcp-handle-expand-many-files.
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
              (rcp-encoding-command     nil)
              (rcp-decoding-command     nil)
              (rcp-encoding-function    nil)
              (rcp-decoding-function    nil)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       nil))
     ("scp"   (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh")
              (rcp-rcp-program          "scp")
              (rcp-rsh-args             ("-e" "none"))
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    "-p")
              (rcp-encoding-command     nil)
              (rcp-decoding-command     nil)
              (rcp-encoding-function    nil)
              (rcp-decoding-function    nil)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       nil))
     ("scp1"  (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh1")
              (rcp-rcp-program          "scp1")
              (rcp-rsh-args             ("-e" "none"))
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    "-p")
              (rcp-encoding-command     nil)
              (rcp-decoding-command     nil)
              (rcp-encoding-function    nil)
              (rcp-decoding-function    nil)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       nil))
     ("scp2"  (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh2")
              (rcp-rcp-program          "scp2")
              (rcp-rsh-args             ("-e" "none"))
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    "-p")
              (rcp-encoding-command     nil)
              (rcp-decoding-command     nil)
              (rcp-encoding-function    nil)
              (rcp-decoding-function    nil)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       nil))
     ("rsync" (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh")
              (rcp-rcp-program          "rsync")
              (rcp-rsh-args             ("-e" "none"))
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    "-t")
              (rcp-encoding-command     nil)
              (rcp-decoding-command     nil)
              (rcp-encoding-function    nil)
              (rcp-decoding-function    nil)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       nil))
     ("ru"    (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "rsh")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "uuencode")
              (rcp-decoding-command     "uudecode -p")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       nil))
     ("su"    (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "uuencode")
              (rcp-decoding-command     "uudecode -p")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       nil))
     ("su1"   (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh1")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "uuencode")
              (rcp-decoding-command     "uudecode -p")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       nil))
     ("su2"   (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh2")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "uuencode")
              (rcp-decoding-command     "uudecode -p")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       nil))
     ("rm"    (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "rsh")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       nil))
     ("sm"    (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       nil))
     ("sm1"   (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh1")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       nil))
     ("sm2"   (rcp-connection-function  rcp-open-connection-rsh)
              (rcp-rsh-program          "ssh2")
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       nil))
     ("tm"    (rcp-connection-function  rcp-open-connection-telnet)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       "telnet")
              (rcp-rlogin-program       nil))
     ("tu"    (rcp-connection-function  rcp-open-connection-telnet)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "uuencode")
              (rcp-decoding-command     "uudecode -p")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       "telnet")
              (rcp-rlogin-program       nil))
     ("rlm"   (rcp-connection-function  rcp-open-connection-rlogin)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       "rlogin"))
     ("rlu"   (rcp-connection-function  rcp-open-connection-rlogin)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "uuencode")
              (rcp-decoding-command     "uudecode -p")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       "rlogin"))
     ("slm"   (rcp-connection-function  rcp-open-connection-rlogin)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       "slogin"))
     ("slm1"  (rcp-connection-function  rcp-open-connection-rlogin)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       "slogin1"))
     ("slm2"  (rcp-connection-function  rcp-open-connection-rlogin)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "mimencode -b")
              (rcp-decoding-command     "mimencode -u -b")
              (rcp-encoding-function    base64-encode-region)
              (rcp-decoding-function    base64-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       "slogin2"))
     ("slu"   (rcp-connection-function  rcp-open-connection-rlogin)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "uuencode")
              (rcp-decoding-command     "uudecode -p")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       "slogin"))
     ("slu1"  (rcp-connection-function  rcp-open-connection-rlogin)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "uuencode")
              (rcp-decoding-command     "uudecode -p")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       "slogin1"))
     ("slu2"  (rcp-connection-function  rcp-open-connection-rlogin)
              (rcp-rsh-program          nil)
              (rcp-rcp-program          nil)
              (rcp-rsh-args             nil)
              (rcp-rcp-args             nil)
              (rcp-rcp-keep-date-arg    nil)
              (rcp-encoding-command     "uuencode")
              (rcp-decoding-command     "uudecode -p")
              (rcp-encoding-function    nil)
              (rcp-decoding-function    uudecode-decode-region)
              (rcp-telnet-program       nil)
              (rcp-rlogin-program       "slogin2"))
     )
  "*Alist of methods for remote files.
This is a list of entries of the form (name parm1 parm2 ...).
Each name stands for a remote access method.  Each parameter is a
pair of the form (key value).  The following keys are defined:
  * rcp-connection-function
    This specifies the function to use to connect to the remote host.
    Currently, `rcp-open-connection-rsh', `rcp-open-connection-telnet'
    and `rcp-open-connection-rlogin' are defined.
    The difference between `rcp-open-connection-rsh' is that it expects
    that you do not need to enter a password to log in, whereas
    `rcp-open-connection-rlogin' does expect you to enter a password.
  * rcp-rsh-program
    This specifies the name of the program to use for rsh; this might be
    the full path to rsh or the name of a workalike program.
  * rcp-rsh-args
    This specifies the list of arguments to pass to the above mentioned
    program.  Please note that this is a list of arguments, that is,
    normally you don't want to put \"-a -b\" here.  Instead, you want
    two list elements, one for \"-a\" and one for \"-b\".
  * rcp-rcp-program
    This specifies the name of the program to use for rcp; this might be
    the full path to rcp or the name of a workalike program.
  * rcp-rcp-args
    This specifies the list of parameters to pass to the above mentioned
    program, the hints for `rcp-rsh-args' also apply here.
  * rcp-rcp-keep-date-arg
    This specifies the parameter to use for `rcp' when the timestamp
    of the original file should be kept.  For `rcp', use `-p', for
    `rsync', use `-t'.
  * rcp-encoding-command
    This specifies a command to use to encode the file contents for
    transfer.  The encoded file contents should go to stdout.
    In this string, the percent escape \"%f\" should be used to indicate
    the file to convert.  \"%%\" is replaced with \"%\".
  * rcp-decoding-command
    This specifies a command to use to decode file contents encoded with
    `rcp-encoding-command'.  The command should read stdin and write to
    stdout.
  * rcp-encoding-function
    This specifies a function to be called to encode the file contents
    on the local side.  This function should encode the region, accepting
    two arguments, start and end.
  * rcp-decoding-function
    Same for decoding on the local side.
  * rcp-telnet-program
    Specifies the telnet program to use when using
    `rcp-open-connection-telnet' to log in.
  * rcp-rlogin-program
    Specifies the rlogin program to use when using
    `rcp-open-connection-rlogin' to log in.

What does all this mean?  Well, you should specify `rcp-rsh-program' for all
methods; this program is used to log in to the remote site.  Then, there are
two ways to actually transfer the files between the local and the remote
side.  One way is using an additional rcp-like program.  If you want to do
this, set `rcp-rcp-program' in the method.

Another possibility for file transfer is inline transfer, i.e. the file
is passed through the same buffer used by `rcp-rsh-program'.  In this case,
the file contents need to be protected since the `rcp-rsh-program' might
use escape codes or the connection might not be eight-bit clean.  Therefore,
file contents are encoded for transit.

Two possibilities for encoding are uuencode/uudecode and mimencode.
For uuencode/uudecode you want to set `rcp-encoding-command' to something
like \"uuencode\" and `rcp-decoding-command' to \"uudecode -p\".
For mimencode you want to set `rcp-encoding-command' to something like
\"mimencode -b\" and `rcp-decoding-command' to \"mimencode -b -u\".

When using inline transfer, you can use a program or a Lisp function
on the local side to encode or decode the file contents.  Set the
`rcp-encoding-function' and `rcp-decoding-function' parameters to nil
in order to use the commands or to the function to use.  It is
possible to specify one function and the other parameter as nil."
  :group 'rcp
  :type '(repeat
          (cons string
                (set (list (const rcp-connection-function) function)
                     (list (const rcp-rsh-program) string)
                     (list (const rcp-rcp-program) string)
                     (list (const rcp-rsh-args) (repeat string))
                     (list (const rcp-rcp-args) (repeat string))
                     (list (const rcp-rcp-keep-date-arg) string)
                     (list (const rcp-encoding-command) string)
                     (list (const rcp-decoding-command) string)
                     (list (const rcp-encoding-function) function)
                     (list (const rcp-decoding-function) function)
                     (list (const rcp-telnet-program) string)
                     (list (const rcp-rlogin-program) string)))))

(defcustom rcp-default-method "rcp"
  "*Default method to use for transferring files.
See `rcp-methods' for possibilities."
  :group 'rcp
  :type 'string)

(defcustom rcp-connection-function 'rcp-open-connection-rsh
  "*Default connection function.
This is used if the like-named parameter isn't specified in `rcp-methods'.
Might be the name of a workalike program, or include the full path."
  :group 'rcp
  :type 'function)

(defcustom rcp-rsh-program "rsh"
  "*Default name of rsh program.
This is used if the like-named parameter isn't specified in `rcp-methods'.
Might be the name of a workalike program, or include the full path."
  :group 'rcp
  :type 'string)

(defcustom rcp-rsh-args nil
  "*Args for running rsh (`rcp-rsh-program', actually.)
This is used if the like-named parameter isn't specified in `rcp-methods'.
User name and host name are always passed, as in `rsh -l jrl remhost command'.
This variable specifies additional arguments only.
This should be a list of strings, each word one element.  For example,
if you wanted to pass `-e none', then you would set this to (\"-e\" \"none\")."
  :group 'rcp
  :type '(repeat string))

(defcustom rcp-rcp-program "rcp"
  "*Name of rcp program.
This is used if the like-named parameter isn't specified in `rcp-methods'.
Might be the name of a workalike program, or include the full path.
Please try this from the command line; the manual page says that `rcp'
easily gets confused by output from ~/.profile or equivalent."
  :group 'rcp
  :type 'string)

(defcustom rcp-rcp-args nil
  "*Args for running rcp.
This is used if the like-named parameter isn't specified in `rcp-methods'.
This is similar to `rcp-rsh-args'."
  :group 'rcp
  :type '(repeat string))

(defcustom rcp-rcp-keep-date-arg nil
  "*Arg for telling rcp to keep timestamp.
This is used if the like-named parameter isn't specified in `rcp-methods'."
  :group 'rcp
  :type 'string)

(defcustom rcp-encoding-command nil
  "*Remote command to use for encoding file contents.
This is used if the like-named parameter isn't specified in `rcp-methods'.
\"%t\" is replaced with tmp file name, \"%f\" is replaced with remote file
name.
Possible values are \"uuencode %t %f\" and \"mimencode %f\".
The command is sent to the remote shell, it must contain the \"%f\" escape
to indicate the remote file name to encode.  It may also contain the \"%t\"
escape in case the output of the encoder contains a file name for the
decoder to use."
  :group 'rcp
  :type 'string)

(defcustom rcp-decoding-command nil
  "*Local program to use for decoding file contents.
This is used if the like-named parameter isn't specified in `rcp-methods'.
Possible values are \"uudecode -p\" and \"mimencode -b -u\"."
  :group 'rcp
  :type 'string)

(defcustom rcp-encoding-function nil
  "*See `rcp-methods'."
  :group 'rcp
  :type 'function)

(defcustom rcp-decoding-function nil
  "*See `rcp-methods'."
  :group 'rcp
  :type 'function)

(defcustom rcp-telnet-program "telnet"
  "*See `rcp-methods'."
  :group 'rcp
  :type 'string)

(defcustom rcp-rlogin-program "rlogin"
  "*See `rcp-methods'."
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

(defcustom rcp-temp-name-prefix "rcp."
  "*Prefix to use for temporary files.
If this is a relative file name (such as \"rcp.\"), it is considered relative
to `(rcp-temporary-file-directory)'.  It may also be an absolute file name;
don't forget to include a prefix for the filename part, though."
  :group 'rcp
  :type 'string)

;; File name format.

(defcustom rcp-file-name-structure
  (list "\\`/r\\(@\\([a-z0-9]+\\)\\)?:\\(\\([a-z0-9_]+\\)@\\)?\\([a-z0-9.-]+\\):\\(.*\\)\\'"
        2 4 5 6)
  "*List of five elements, detailing the rcp file name structure.

The first element is a regular expression matching an rcp file name.
The regex should contain parentheses around the method name, the user
name, the host name, and the file name parts.

The second element is a number, saying which pair of parentheses
matches the method name.  The third element is similar, but for the
user name.  The fourth element is similar, but for the host name.  The
fifth element is for the file name.  These numbers are passed directly
to `match-string', which see.  That means the opening parentheses are
counted to identify the pair.

See also `rcp-file-name-regexp' and `rcp-make-rcp-file-format'."
  :group 'rcp
  :type '(list (regexp :tag "File name regexp")
               (integer :tag "Paren pair for method name")
               (integer :tag "Paren pair for user name  ")
               (integer :tag "Paren pair for host name  ")
               (integer :tag "Paren pair for file name  ")))

(defcustom rcp-file-name-regexp "\\`/r[@:]"
  "*Regular expression matching rcp file names.
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
%m is replaced by the method name.
%u is replaced by the user name.
%h is replaced by the host name.
%p is replaced by the file name.
%% is replaced by %.

Also see `rcp-file-name-structure' and `rcp-file-name-regexp'."
  :group 'rcp
  :type 'string)

;;; Internal Variables:

(defvar rcp-end-of-output "/////"
  "String used to recognize end of output.")

(defvar rcp-ls-command nil
  "This command is used to get a long listing with numeric user and group ids.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar rcp-current-method nil
  "Contains connection method for this *rcp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar rcp-current-user nil
  "Contains remote login name for this *rcp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

(defvar rcp-current-host nil
  "Contains remote host for this *rcp* buffer.
This variable is automatically made buffer-local to each rsh process buffer
upon opening the connection.")

;; New handlers should be added here.  The following operations can be
;; handled using the normal primitives: file-name-as-directory,
;; file-name-directory, file-name-nondirectory,
;; file-name-sans-versions, get-file-buffer.
(defconst rcp-file-name-handler-alist
  '((file-exists-p . rcp-handle-file-exists-p)
    (file-directory-p . rcp-handle-file-directory-p)
    (file-executable-p . rcp-handle-file-executable-p)
    (file-accessible-directory-p . rcp-handle-file-accessible-directory-p)
    (file-readable-p . rcp-handle-file-readable-p)
    (file-regular-p . rcp-handle-file-regular-p)
    (file-symlink-p . rcp-handle-file-symlink-p)
    (file-writable-p . rcp-handle-file-writable-p)
    (file-attributes . rcp-handle-file-attributes)
    (file-directory-files . rcp-handle-file-directory-files)
    (file-name-all-completions . rcp-handle-file-name-all-completions)
    (file-name-completion . rcp-handle-file-name-completion)
    (add-name-to-file . rcp-handle-add-name-to-file)
    (copy-file . rcp-handle-copy-file)
    (make-directory . rcp-handle-make-directory)
    (delete-directory . rcp-handle-delete-directory)
    (delete-file . rcp-handle-delete-file)
    (directory-file-name . rcp-handle-directory-file-name)
    (dired-call-process . rcp-handle-dired-call-process)
    (shell-command . rcp-handle-shell-command)
    (insert-directory . rcp-handle-insert-directory)
    (expand-file-name . rcp-handle-expand-file-name)
    (file-local-copy . rcp-handle-file-local-copy)
    (insert-file-contents . rcp-handle-insert-file-contents)
    (write-region . rcp-handle-write-region)
    (vc-registered . rcp-handle-vc-registered))
        "Alist of handler functions.
Operations not mentioned here will be handled by the normal Emacs functions.")

;;; Internal functions which must come first.

(defsubst rcp-message (level fmt-string &rest args)
  (when (<= level rcp-verbose)
    (apply #'message fmt-string args)
    (when rcp-debug-buffer
      (save-excursion
        (set-buffer
         (rcp-get-debug-buffer rcp-current-method
                               rcp-current-user rcp-current-host))
        (goto-char (point-max))
        (insert "# " (apply #'format fmt-string args) "\n")))))

;; Extract right value of alists, depending on host name.

(defsubst rcp-alist-get (string alist)
  "Return the value from the alist, based on regex matching against the keys."
  (cdr (assoc* string alist
               :test (function (lambda (a b)
                                 (string-match b a))))))

;;; File Name Handler Functions:

;; Basic functions.

(defun rcp-handle-file-exists-p (filename)
  "Like `file-exists-p' for rcp files."
  (let ((v (rcp-dissect-file-name filename))
        method user host path)
    (setq method (rcp-file-name-method v))
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (rcp-send-command method user host
                        (format "%s -d %s >/dev/null 2>&1"
                                (rcp-get-ls-command method user host)
                                (shell-quote-argument path)))
      (rcp-send-command method user host "echo $?")
      (rcp-wait-for-output)
      (zerop (read (current-buffer))))))

(defun rcp-handle-file-attributes (filename)
  "Like `file-attributes' for rcp files."
  (let ((v (rcp-dissect-file-name filename))
        user host path symlinkp dirp
        res-inode res-filemodes res-numlinks
        res-uid res-gid res-size res-symlink-target)
    (if (not (rcp-handle-file-exists-p filename))
        nil                             ; file cannot be opened
      ;; file exists, find out stuff
      (save-excursion
        (rcp-send-command
         (rcp-file-name-method v)
         (rcp-file-name-user v) (rcp-file-name-host v)
         (format "%s -iLldn %s"
                 (rcp-get-ls-command (rcp-file-name-method v)
                                     (rcp-file-name-user v)
                                     (rcp-file-name-host v))
                 (shell-quote-argument (rcp-file-name-path v))))
        (rcp-wait-for-output)
        ;; parse `ls -l' output ...
        ;; ... inode
        (setq res-inode (read (current-buffer)))
        ;; ... file mode flags
        (setq res-filemodes (symbol-name (read (current-buffer))))
        ;; ... number links
        (setq res-numlinks (read (current-buffer)))
        ;; ... uid and gid
        (setq res-uid (read (current-buffer)))
        (setq res-gid (read (current-buffer)))
        (unless (numberp res-uid) (setq res-uid -1))
        (unless (numberp res-gid) (setq res-gid -1))
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
                                  (progn (end-of-line) (point))))))
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

(defun rcp-handle-file-directory-p (filename)
  (eq t (car (rcp-handle-file-attributes filename))))

(defun rcp-handle-file-regular-p (filename)
  "Like `file-regular-p' for rcp files."
  (eq ?- (aref (nth 8 (rcp-handle-file-attributes filename)) 0)))

(defun rcp-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for rcp files."
  (stringp (car (rcp-handle-file-attributes filename))))

(defun rcp-handle-file-writable-p (filename)
  "Like `file-writable-p' for rcp files."
  (if (rcp-handle-file-exists-p filename)
      ;; Existing files must be writable.
      (zerop (rcp-run-test "-w" filename))
    ;; If file doesn't exist, check if directory is writable.
    (and (zerop (rcp-run-test "-d" (file-name-directory filename)))
         (zerop (rcp-run-test "-w" (file-name-directory filename))))))
       

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
  (let ((v (rcp-dissect-file-name directory))
        method user host path result x)
    (setq method (rcp-file-name-method v))
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (rcp-send-command method user host
                        (concat "cd " (shell-quote-argument path)
                                " ; echo $?"))
      (rcp-barf-unless-okay
       "rcp-handle-directory-files: couldn't cd %s"
       (shell-quote-argument path))
      (rcp-send-command
       method user host
       (concat (rcp-get-ls-command method user host) " -a"))
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
            (push x result)))))
    result))

;; This function should return "foo/" for directories and "bar" for
;; files.  We use `ls -ad *' to get a list of files (including
;; directories), and `ls -ad */' to get a list of directories.
(defun rcp-handle-file-name-all-completions (file directory)
  "Like `file-name-all-completions' for rcp files."
  (let ((v (rcp-dissect-file-name directory))
        method user host path dirs result)
    (setq method (rcp-file-name-method v))
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (rcp-send-command method user host
                        (format "cd %s ; echo $?" (shell-quote-argument path)))
      (rcp-barf-unless-okay
       "rcp-handle-file-name-all-completions: Couldn't cd %s"
       (shell-quote-argument path))
      ;; Get list of file names by calling ls.
      (rcp-send-command method user host
                        (format "%s -a 2>/dev/null"
                                (rcp-get-ls-command method user host)))
      (rcp-wait-for-output)
      (goto-char (point-max))
      (while (zerop (forward-line -1))
        (push (buffer-substring (point)
                                (progn (end-of-line) (point)))
              result))
      ;; Now get a list of directories in a similar way.
      (rcp-send-command method user host
                        (format "%s -d .*/ */ 2>/dev/null"
                                (rcp-get-ls-command method user host)
                                (shell-quote-argument file)))
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
     (all-completions file (mapcar (lambda (x) (list x)) result)))))

;; The following isn't needed for Emacs 20 but for 19.34?
(defun rcp-handle-file-name-completion (file directory)
  "Like `file-name-completion' for rcp files."
  (unless (rcp-rcp-file-p directory)
    (error "rcp-handle-file-name-completion invoked on non-rcp directory: %s"
           directory))
  (try-completion
   file
   (mapcar (lambda (x) (cons x nil))
           (rcp-handle-file-name-all-completions file directory))))

;; cp, mv and ln

(defun rcp-handle-add-name-to-file
  (file newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for rcp files."
  (let* ((v1 (when (rcp-rcp-file-p file)
               (rcp-dissect-file-name file)))
         (v2 (when (rcp-rcp-file-p newname)
               (rcp-dissect-file-name newname)))
         (meth1 (when v1 (rcp-file-name-method v1)))
         (meth2 (when v2 (rcp-file-name-method v2)))
         (user1 (when v1 (rcp-file-name-user v1)))
         (user2 (when v2 (rcp-file-name-user v2)))
         (host1 (when v1 (rcp-file-name-host v1)))
         (host2 (when v2 (rcp-file-name-host v2)))
         (path1 (when v1 (rcp-file-name-path v1)))
         (path2 (when v2 (rcp-file-name-path v2))))
    (unless (and meth1 meth2 user1 user2 host1 host2
                 (string= meth1 meth2)
                 (string= user1 user2)
                 (string= host1 host2))
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
    (rcp-send-command meth1 user1 host1
                      (format "ln %s %s ; echo $?"
                              (shell-quote-argument path1)
                              (shell-quote-argument path2)))
    (rcp-barf-unless-okay
     "error with add-name-to-file, see buffer `%s' for details"
     (buffer-name))))

(defun rcp-handle-copy-file
  (file newname &optional ok-if-already-exists keep-date)
  "Like `copy-file' for rcp files."
  ;; Check if both files are local -- invoke normal copy-file.
  ;; Otherwise, use rcp from local system.
  (setq file (expand-file-name file))
  (setq newname (expand-file-name newname))
  ;; At least one file an rcp file?
  (if (or (rcp-rcp-file-p file)
          (rcp-rcp-file-p newname))
      (rcp-do-copy-file file newname ok-if-already-exists keep-date)
    (rcp-run-real-handler 'copy-file
                           (list file newname ok-if-already-exists))))

(defun rcp-do-copy-file
  (file newname &optional ok-if-already-exists keep-date)
  "Invoked by `rcp-handle-copy-file' to actually do the copying.
FILE and NEWNAME must be absolute file names."
  (unless ok-if-already-exists
    (when (file-exists-p newname)
      (signal 'file-already-exists
              (list newname))))
  (let* ((v1 (when (rcp-rcp-file-p file)
               (rcp-dissect-file-name file)))
         (v2 (when (rcp-rcp-file-p newname)
               (rcp-dissect-file-name newname)))
         (meth1 (when v1 (rcp-file-name-method v1)))
         (meth2 (when v2 (rcp-file-name-method v2)))
         (meth (rcp-file-name-method (or v1 v2)))
         (rcp-program (rcp-get-rcp-program meth))
         (rcp-args (rcp-get-rcp-args meth)))
    (if (and meth1 meth2 (string= meth1 meth2)
             (string= (rcp-file-name-host v1)
                      (rcp-file-name-host v2))
             (string= (rcp-file-name-user v1)
                      (rcp-file-name-user v2)))
        ;; If method, host, user are the same for both files, we
        ;; invoke `cp' on the remote host directly.
        (rcp-do-copy-file-directly
         (rcp-file-name-method v1)
         (rcp-file-name-user v1)
         (rcp-file-name-host v1)
         (rcp-file-name-path v1) (rcp-file-name-path v2)
         keep-date)
      ;; In all other cases, we rely on the rcp program to do the
      ;; right thing -- barf if not using rcp.
      (unless rcp-program
        (error "Cannot `copy-file' for methods with inline copying -- yet."))
      (let ((f1 (if (not v1)
                    file
                  (rcp-make-rcp-program-file-name
                   (rcp-file-name-user v1)
                   (rcp-file-name-host v1)
                   (shell-quote-argument (rcp-file-name-path v1)))))
            (f2 (if (not v2)
                    newname
                  (rcp-make-rcp-program-file-name
                   (rcp-file-name-user v2)
                   (rcp-file-name-host v2)
                   (shell-quote-argument (rcp-file-name-path v2))))))
        (when keep-date
          (add-to-list 'rcp-args (rcp-get-rcp-keep-date-arg meth)))
        (apply #'call-process (rcp-get-rcp-program meth) nil nil nil
               (append rcp-args (list f1 f2)))))))

(defun rcp-do-copy-file-directly (method user host path1 path2 keep-date)
  "Invokes `cp' on the remote system to copy one file to another."
  (rcp-send-command
   method user host
   (format (if keep-date "cp -p %s %s" "cp %s %s")
           (shell-quote-argument path1)
           (shell-quote-argument path2))))

;; mkdir
(defun rcp-handle-make-directory (dir &optional parents)
  "Like `make-directory' for rcp files."
  (let ((v (rcp-dissect-file-name dir)))
    (rcp-send-command
     (rcp-file-name-method v) (rcp-file-name-user v) (rcp-file-name-host v)
     (format "%s %s"
             (if parents "mkdir -p" "mkdir")
             (shell-quote-argument (rcp-file-name-path v))))))

;; CCC error checking?
(defun rcp-handle-delete-directory (directory)
  "Like `delete-directory' for rcp files."
  (let ((v (rcp-dissect-file-name directory))
        result)
    (save-excursion
      (rcp-send-command
       (rcp-file-name-method v) (rcp-file-name-user v) (rcp-file-name-host v)
       (format "rmdir %s ; echo ok"
               (shell-quote-argument (rcp-file-name-path v))))
      (rcp-wait-for-output))))

(defun rcp-handle-delete-file (filename)
  "Like `delete-file' for rcp files."
  (let ((v (rcp-dissect-file-name filename))
        result)
    (save-excursion
      (rcp-send-command
       (rcp-file-name-method v)
       (rcp-file-name-user v)
       (rcp-file-name-host v)
       (format "rm -f %s ; echo ok"
               (shell-quote-argument (rcp-file-name-path v))))
      (rcp-wait-for-output))))

;; Dired.

(defun rcp-handle-dired-call-process (program discard &rest arguments)
  "Like `dired-call-process' for rcp files."
  (let ((v (rcp-dissect-file-name default-directory))
        method user host path result)
    (setq method (rcp-file-name-method v))
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (rcp-send-command method user host
                        (format "cd %s ; echo $?" (shell-quote-argument path)))
      (rcp-barf-unless-okay
       "rcp-handle-dired-call-process: Couldn't cd %s"
       (shell-quote-argument path))
      (rcp-send-command method user host
                        (mapconcat #'identity (cons program arguments) " "))
      (rcp-wait-for-output))
    (unless discard
      (insert-buffer (rcp-get-buffer method user host)))
    (save-excursion
      (rcp-send-command method user host "echo $?")
      (rcp-wait-for-output)
      (read (rcp-get-buffer method user host)))))

;; Pacify byte-compiler.  The function is needed on XEmacs only.  I'm
;; not sure at all that this is the right way to do it, but let's hope
;; it works for now, and wait for a guru to point out the Right Way to
;; achieve this.
(eval-when-compile
  (unless (fboundp 'dired-insert-set-properties)
    (fset 'dired-insert-set-properties 'ignore)))

(defun rcp-handle-insert-directory
  (file switches &optional wildcard full-directory-p)
  "Like `insert-directory' for rcp files."
  (let* ((f (expand-file-name file))
         (v (rcp-dissect-file-name f))
         method user host path)
    (setq method (rcp-file-name-method v))
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (when (listp switches)
      (setq switches (mapconcat #'identity switches " ")))
    (unless full-directory-p
      (setq switches (concat "-d " switches)))
    (save-excursion
      (if (not (file-directory-p file))
          ;; Just do `ls -l /tmp/foo' for files.
          (rcp-send-command method user host
                            (format "%s %s %s"
                                    (rcp-get-ls-command method user host)
                                    switches
                                    (shell-quote-argument path)))
        ;; Do `cd /dir' then `ls -l' for directories.
        (rcp-send-command
         method user host
         (format "cd %s ; echo $?" (shell-quote-argument path)))
        (rcp-barf-unless-okay
         "rcp-handle-insert-directory: Couldn't cd %s"
         (shell-quote-argument path))
        (rcp-send-command method user host
                          (format "%s %s"
                                  (rcp-get-ls-command method user host)
                                  switches)))
      (sit-for 1)                       ;needed for rsh but not ssh?
      (rcp-wait-for-output))
    (insert-buffer (rcp-get-buffer method user host))
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
(eval-when-compile
  (when (eq (symbol-function 'dired-insert-set-properties) 'ignore)
    (fmakunbound 'dired-insert-set-properties)))

;; Canonicalization of file names.

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
               method user host
               (format "cd %s; pwd" uname))
              (rcp-wait-for-output)
              (goto-char (point-min))
              (setq uname (buffer-substring (point)
                                            (progn (end-of-line)
                                                   (point))))
              (setq path (concat uname fname)))) ;)
          ;; No tilde characters in file name, do normal
          ;; expand-file-name (this does "/./" and "/../").
          (rcp-make-rcp-file-name
           method user host
           (rcp-run-real-handler 'expand-file-name (list path))))))))

;; Remote commands.

(defun rcp-handle-shell-command (command &optional output-buffer)
  "Like `shell-command' for rcp files.
Bug: COMMAND must not output the string `/////'.
Bug: output of COMMAND must end with a newline."
  (if (rcp-rcp-file-p default-directory)
      (let* ((v (rcp-dissect-file-name default-directory))
             (method (rcp-file-name-method v))
             (user (rcp-file-name-user v))
             (host (rcp-file-name-host v))
             (path (rcp-file-name-path v)))
        (when (string-match "&[ \t]*\\'" command)
          (error "Rcp doesn't grok asynchronous shell commands, yet."))
        (save-excursion
          (rcp-send-command
           method user host
           (format "cd %s; echo $?" (shell-quote-argument path)))
          (rcp-barf-unless-okay
           "rcp-handle-shell-command: Couldn't cd %s"
           (shell-quote-argument path))
          (rcp-send-command method user host
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
        (insert-buffer (rcp-get-buffer method user host))
        (save-excursion
          (rcp-send-command
           method user host
           "rcp_set_exit_status $rcp_old_status"))
        (unless (zerop (buffer-size))
          (pop-to-buffer output-buffer)))
    ;; The following is only executed if something strange was
    ;; happening.  Emit a helpful message and do it anyway.
    (message "rcp-handle-shell-command called with non-rcp directory: %s"
             default-directory)
    (rcp-run-real-handler 'shell-command
                           (list command output-buffer))))

;; File Editing.

(defun rcp-handle-file-local-copy (filename)
  "Like `file-local-copy' for rcp files."
  (let* ((v (rcp-dissect-file-name filename))
         (method (rcp-file-name-method v))
         (user (rcp-file-name-user v))
         (host (rcp-file-name-host v))
         (path (rcp-file-name-path v))
         tmpfil)
    (unless (file-exists-p filename)
      (error "rcp-handle-file-local-copy: file %s does not exist!"
             filename))
    (setq tmpfil (make-temp-name
                  (expand-file-name rcp-temp-name-prefix
                                    (rcp-temporary-file-directory))))
    (cond ((rcp-get-rcp-program method)
           ;; Use rcp-like program for file transfer.
           (rcp-message 5 "Fetching %s to tmp file..." filename)
           (apply #'call-process
                  (rcp-get-rcp-program method)
                  nil nil nil
                  (append (rcp-get-rcp-args method)
                          (list
                           (rcp-make-rcp-program-file-name
                            user host
                            (shell-quote-argument path))
                           tmpfil)))
           (rcp-message 5 "Fetching %s to tmp file...done" filename))
          ((and (rcp-get-encoding-command method)
                (rcp-get-decoding-command method))
           ;; Use inline encoding for file transfer.
           (save-excursion
             (rcp-message 5 "Encoding remote file %s..." filename)
             (rcp-send-command
              method user host
              (concat (rcp-get-encoding-command method)
                      " < " (shell-quote-argument path)
                      "; echo $?"))
             (rcp-barf-unless-okay
              "Encoding remote file failed, see buffer %S for details."
              (rcp-get-buffer method user host))
             ;; Remove trailing status code
             (goto-char (point-max))
             (delete-region (point) (progn (forward-line -1) (point)))
             
             (rcp-message 5 "Decoding remote file %s..." filename)
             (if (and (rcp-get-decoding-function method)
                      (fboundp (rcp-get-decoding-function method)))
                 ;; If rcp-decoding-function is defined for this
                 ;; method, we call it.
                 (let ((tmpbuf (get-buffer-create " *rcp tmp*")))
                   (set-buffer tmpbuf)
                   (erase-buffer)
                   (insert-buffer (rcp-get-buffer method user host))
                   (rcp-message
                    6 "Decoding remote file %s with function %s..."
                    filename
                    (rcp-get-decoding-function method))
                   (set-buffer tmpbuf)
                   (funcall (rcp-get-decoding-function method)
                            (point-min)
                            (point-max))
                   (write-region (point-min) (point-max) tmpfil)
                   (kill-buffer tmpbuf))
               ;; If rcp-decoding-function is not defined for this
               ;; method, we invoke rcp-decoding-command instead.
             (let ((tmpfil2
                    (make-temp-name
                     (expand-file-name rcp-temp-name-prefix
                                       (rcp-temporary-file-directory)))))
               (write-region (point-min) (point-max) tmpfil2)
               (rcp-message
                6 "Decoding remote file %s with command %s..."
                filename
                (rcp-get-decoding-command method))
               (call-process
                "/bin/sh"
                tmpfil2                 ;input
                nil                     ;output
                nil                     ;display
                "-c" (concat (rcp-get-decoding-command method)
                             " > " tmpfil))
               (delete-file tmpfil2)))
             (rcp-message 5 "Decoding remote file %s...done" filename)))

          (t (error "Wrong method specification for %s." method)))
    tmpfil))


;; CCC need to do MULE stuff
(defun rcp-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for rcp files."
  (if (not (file-exists-p filename))
      (progn
        (when visit
          (setq buffer-file-name filename)
          (set-visited-file-modtime '(0 0))
          (set-buffer-modified-p nil)
          (when auto-save-default
            (auto-save-mode 1)
            (when rcp-auto-save-directory
              (setq buffer-auto-save-file-name
                    (rcp-make-auto-save-name filename)))))
        (list (expand-file-name filename) 0))
    (let ((local-copy (rcp-handle-file-local-copy filename))
          (result nil))
      (when visit
        (setq buffer-file-name filename)
        (set-visited-file-modtime '(0 0))
        (set-buffer-modified-p nil)
        ;; Is this the right way to go about auto-saving?
        (when auto-save-default
          (auto-save-mode 1)
          (when rcp-auto-save-directory
            (setq buffer-auto-save-file-name
                  (rcp-make-auto-save-name filename)))))
      (setq result
            (rcp-run-real-handler 'insert-file-contents
                                  (list local-copy nil beg end replace)))
      (delete-file local-copy)
      (list (expand-file-name filename)
            (second result))
      )))

;; CCC grok APPEND, LOCKNAME, CONFIRM
(defun rcp-handle-write-region
  (start end filename &optional append visit lockname confirm)
  "Like `write-region' for rcp files."
  (unless (eq append nil)
    (error "rcp-handle-write-region: APPEND must be nil."))
  (unless (or (eq lockname nil)
              (string= lockname filename))
    (error "rcp-handle-write-region: LOCKNAME must be nil or equal FILENAME."))
  (when (and confirm (file-exists-p filename))
    (unless (y-or-n-p (format "File %s exists; overwrite anyway? "
                              filename))
      (error "File not overwritten.")))
  (let* ((v (rcp-dissect-file-name filename))
         (method (rcp-file-name-method v))
         (user (rcp-file-name-user v))
         (host (rcp-file-name-host v))
         (path (rcp-file-name-path v))
         (rcp-program (rcp-get-rcp-program method))
         (rcp-args (rcp-get-rcp-args method))
         (encoding-command (rcp-get-encoding-command method))
         (encoding-function (rcp-get-encoding-function method))
         (decoding-command (rcp-get-decoding-command method))
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
    (setq tmpfil
          (make-temp-name (expand-file-name rcp-temp-name-prefix
                                            (rcp-temporary-file-directory))))
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
           (apply #'call-process
                  rcp-program nil nil nil
                  (append rcp-args
                          (list
                           tmpfil
                           (rcp-make-rcp-program-file-name
                            user host
                            (shell-quote-argument path))))))
          ((and encoding-command decoding-command)
           ;; Use inline file transfer
           (let ((tmpbuf (get-buffer-create " *rcp file transfer*")))
             (save-excursion
               ;; Encode tmpfil into tmpbuf
               (rcp-message 5 "Encoding region...")
               (set-buffer tmpbuf)
               (erase-buffer)
               ;; Use encoding function or command.
               (if (and encoding-function
                        (fboundp encoding-function))
                   (progn
                     (rcp-message 6 "Encoding region using function...")
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
                 (rcp-message 6 "Encoding region using command...")
                 (call-process
                  "/bin/sh"
                  tmpfil                ;input = local tmp file
                  t                     ;output is current buffer
                  nil                   ;don't redisplay
                  "-c"
                  encoding-command))
               ;; Send tmpbuf into remote decoding command which
               ;; writes to remote file.  Because this happens on the
               ;; remote host, we cannot use the function.
               (rcp-message 5 "Decoding region into remote file %s..."
                            filename)
               (rcp-send-command
                method user host
                (format "%s <<'%s' >%s" ;mkoeppe: must quote EOF delimiter
                        decoding-command
                        rcp-end-of-output
                        (shell-quote-argument path)))
               (set-buffer tmpbuf)
               (rcp-message 6 "Sending data to remote host...")
               (rcp-send-region method user host (point-min) (point-max))
               ;; wait for remote decoding to complete
               (rcp-message 6 "Sending end of data token...")
               (rcp-send-command method user host rcp-end-of-output t)
               (rcp-message 6 "Waiting for remote host to process data...")
               (rcp-send-command method user host "echo hello")
               (set-buffer (rcp-get-buffer method user host))
               (rcp-wait-for-output)
               (rcp-message 5 "Decoding region into remote file %s...done"
                            filename)
               (kill-buffer tmpbuf))))
          (t
           (error
            (concat "Method %s should specify both encoding and "
                    "decoding command or an rcp program.")
            method)))
    (delete-file tmpfil)
    (when visit
      ;; Is this right for auto-saving?
      (when auto-save-default
        (auto-save-mode 1)
        (when rcp-auto-save-directory
          (setq buffer-auto-save-file-name
                (rcp-make-auto-save-name filename)))))
    ;; Make `last-coding-system-used' have the right value.
    (when (boundp 'last-coding-system-used)
      (setq last-coding-system-used coding-system-used))
    (when (or (eq visit t)
              (eq visit nil)
              (stringp visit))
      (message "Wrote %s" filename))))

;; The following is not how the documentation tells us to do things.
;; But Daniel Pittman <daniel@danann.net> reports that this helps
;; to make it work in XEmacs together with EFS.
(defun rcp-run-real-handler (operation args)
  (let ((file-name-handler-alist nil))
    (apply operation args)))
;; The following (commented out) function is what the documentation
;; (of Emacs) says we should use.
;;-(defun rcp-run-real-handler (operation args)
;;-  (let ((inhibit-file-name-handlers
;;-         (cons 'rcp-file-name-handler
;;-               (and (eq inhibit-file-name-operation operation)
;;-                    inhibit-file-name-handlers)))
;;-        (inhibit-file-name-operation operation))
;;-    (apply operation args)))

;; Main function.
(defun rcp-file-name-handler (operation &rest args)
  (let ((fn (assoc operation rcp-file-name-handler-alist)))
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
  (if (or (string-match "\\*" name)
          (string-match "\\?" name)
          (string-match "\\[.*\\]" name))
      (save-excursion
        ;; Dissect NAME.
        (let* ((v (rcp-dissect-file-name name))
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
          ;;-       (set-difference rcp-file-name-quote-list '(?\* ?\? ?[ ?]))))
          ;;-  (rcp-send-command method user host
          ;;-                    (format "echo %s" (comint-quote-filename path)))
          ;;-  (rcp-wait-for-output))
          (rcp-send-command method user host
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
                         (rcp-make-rcp-file-name method user host x)))
             (read (current-buffer))))))
    (list (rcp-handle-expand-file-name name))))

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
(defun rcp-handle-vc-registered (file)
  "Like `vc-registered' for rcp files."
  ;; In this function, we need to take care that no other handlers are
  ;; called -- ange-ftp file names also match rcp file names, so we
  ;; just remove all other file name handlers from the alist and call
  ;; the primitive.
  (let ((file-name-handler-alist nil))
    (rcp-setup-file-name-handler-alist)
    (rcp-run-real-handler 'vc-registered (list file))))

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
  (and file (setq file (expand-file-name file)))
  (if (not buffer) (setq buffer "*vc*"))
  (if vc-command-messages
      (message "Running %s on %s..." command file))
  (let ((obuf (current-buffer)) (camefrom (current-buffer))
	(squeezed nil)
	(olddir default-directory)
	vc-file status)
    (let* ((v (rcp-dissect-file-name file))
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
      (save-excursion
        ;; Actually execute remote command
        (rcp-handle-shell-command
          (mapconcat 'shell-quote-argument
                     (cons command squeezed) " ") t)
        ;(rcp-wait-for-output)
        ;; Get status from command
        (rcp-send-command method user host "echo $?")
        (rcp-wait-for-output)
        (setq status (read (current-buffer)))
        (message "Command %s returned status %d." command status))
      (goto-char (point-max))
      (set-buffer-modified-p nil)
      (forward-line -1)
      (if (or (not (integerp status)) (and okstatus (< okstatus status)))
          (progn
            (pop-to-buffer buffer)
            (goto-char (point-min))
            (shrink-window-if-larger-than-buffer)
            (error "Running %s...FAILED (%s)" command
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

(defadvice vc-do-command
  (around rcp-advice-vc-do-command
          (buffer okstatus command file last &rest flags)
          activate)
  "Invoke rcp-vc-do-command for rcp files."
  (if (and (stringp file) (rcp-rcp-file-p file))
      (setq ad-return-value
            (apply 'rcp-vc-do-command buffer okstatus command file last flags))
    ad-do-it))

;;-  (let ((f (ad-get-arg 3)))
;;-    (if (and (stringp f) (rcp-rcp-file-p f))
;;-        (apply 'rcp-vc-do-command (ad-get-args 0))
;;-      ad-do-it)))

;;-(unless (fboundp 'rcp-original-vc-do-command)
;;-  (fset 'rcp-original-vc-do-command (symbol-function 'vc-do-command)))
;;-
;;-(defun vc-do-command (buffer okstatus command file last &rest flags)
;;-  "Redefined to work with rcp.el; see `rcp-original-vc-do-command' for
;;-original definition."
;;-  (if (and (stringp file) (rcp-rcp-file-p file))
;;-      (apply 'rcp-vc-do-command buffer okstatus command file last flags)
;;-    (apply
;;-     'rcp-original-vc-do-command buffer okstatus command file last flags)))

;; `vc-workfile-unchanged-p'
;; This function does not deal well with remote files, so we do the
;; same as for `vc-do-command'.

;; `vc-workfile-unchanged-p' checks the modification time, we cannot
;; do that for remote files, so here's a version which relies on diff.
(defun rcp-vc-workfile-unchanged-p (file &optional want-differences-if-changed)
  (let ((status (vc-backend-diff file nil nil
                                 (not want-differences-if-changed))))
    (zerop status)))

(defadvice vc-workfile-unchanged-p
  (around rcp-advice-vc-workfile-unchanged-p
          (file &optional want-differences-if-changed)
          activate)
  "Invoke rcp-vc-workfile-unchanged-p for rcp files."
  (if (and (stringp file) (rcp-rcp-file-p file))
      (setq ad-return-value
            (rcp-vc-workfile-unchanged-p file want-differences-if-changed))
    ad-do-it))

;;-(unless (fboundp 'rcp-original-vc-workfile-unchanged-p)
;;-  (fset 'rcp-original-vc-workfile-unchanged-p
;;-        (symbol-function 'vc-workfile-unchanged-p)))
;;-
;;-(defun vc-workfile-unchanged-p (file &optional want-differences-if-changed)
;;-  (if (and (stringp file) (rcp-rcp-file-p file))
;;-      (apply 'rcp-vc-workfile-unchanged-p
;;-             (list file want-differences-if-changed))
;;-    (apply 'rcp-original-vc-workfile-unchanged-p
;;-           (list file want-differences-if-changed))))


;; Redefine a function from vc.el -- allow rcp files.
(defun vc-checkout (file &optional writable rev)
  "Retrieve a copy of the latest version of the given file."
  ;; If ftp is on this system and the name matches the ange-ftp format
  ;; for a remote file, the user is trying something that won't work.
  (if (and (not (rcp-rcp-file-p file))
           (string-match "^/[^/:]+:" file) (vc-find-binary "ftp"))
      (error "Sorry, you can't check out files over FTP"))
  (vc-backend-checkout file writable rev)
  (vc-resynch-buffer file t t))

;;-;; When this function is called from VC, the symbol `file' is bound to
;;-;; the name of the file in question.  Here, we have a gross hack and
;;-;; look to see if we need to determine the user name of a remote file.
;;-(defun vc-user-login-name (&optional uid)
;;-  ;; Return the name under which the user is logged in, as a string.
;;-  ;; (With optional argument UID, return the name of that user.)
;;-  ;; This function does the same as `user-login-name', but unlike
;;-  ;; that, it never returns nil.  If a UID cannot be resolved, that
;;-  ;; UID is returned as a string.
;;-  (let ((caller (backtrace-frame 3)))
;;-    (if (member (cadr caller) (list 'vc-fetch-master-properties
;;-                                    'vc-lock-from-permissions
;;-                                    'vc-file-owner
;;-                                    'vc-fetch-properties
;;-                                    'vc-after-save
;;-                                    'vc-mode-line
;;-                                    'vc-status
;;-                                    'vc-next-action-on-file
;;-                                    'vc-merge
;;-                                    'vc-update-change-log
;;-                                    'vc-backend-checkout
;;-                                    'vc-backend-steal))
;;-        ;; We were called from VC, so we assume that `file' is bound
;;-        ;; and tells us whether we need to do the remote thing.
;;-        (if (and (boundp 'file) file (stringp file)
;;-                 (rcp-rcp-file-p file))
;;-            ;; Okay, let's do the remote thing.
;;-
;;-  (if CCC-
;;-  (or (user-login-name uid)
;;-      (and uid (number-to-string uid))
;;-      (number-to-string (user-uid))))

;;; Internal Functions:

(defun rcp-set-auto-save ()
  (when (and (rcp-rcp-file-p (buffer-file-name))
             auto-save-default)
    (auto-save-mode 1)))
(add-hook 'find-file-hooks 'rcp-set-auto-save t)

(defun rcp-run-test (switch filename)
  "Run `test' on the remote system, given a switch and a file.
Returns the exit code of test."
  (let ((v (rcp-dissect-file-name filename))
        result)
    (save-excursion
      (rcp-send-command
       (rcp-file-name-method v) (rcp-file-name-user v) (rcp-file-name-host v)
       (format "test %s %s ; echo $?" switch
               (shell-quote-argument (rcp-file-name-path v))))
      (rcp-wait-for-output)
      (read (current-buffer)))))

(defun rcp-buffer-name (method user host)
  "A name for the connection buffer for USER at HOST using METHOD."
  (format "*rcp/%s %s@%s*" method user host))

(defun rcp-get-buffer (method user host)
  "Get the connection buffer to be used for USER at HOST using METHOD."
  (get-buffer-create (rcp-buffer-name method user host)))

(defun rcp-debug-buffer-name (method user host)
  "A name for the debug buffer for USER at HOST using METHOD."
  (format "*debug rcp/%s %s@%s*" method user host))

(defun rcp-get-debug-buffer (method user host)
  "Get the debug buffer for USER at HOST using METHOD."
  (get-buffer-create (rcp-debug-buffer-name method user host)))

(defun rcp-find-executable (method user host progname dirlist)
  "Searches for PROGNAME in all directories mentioned in `rcp-remote-path'.
This one expects to be in the right *rcp* buffer."
  (let (result x)
    (while (and (null result) dirlist)
      (setq x (concat (file-name-as-directory (pop dirlist)) progname))
      (rcp-message 5 "Looking for remote executable %s" x)
      (when (rcp-handle-file-executable-p
             (rcp-make-rcp-file-name method user host x))
        (setq result x)))
    (rcp-message 5 "Found remote executable %s" result)
    result))

(defun rcp-set-remote-path (method user host var dirlist)
  "Sets the remote environment VAR to existing directories from DIRLIST.
I.e., for each directory in DIRLIST, it is tested whether it exists and if
so, it is added to the environment variable VAR."
  (rcp-send-command
   method user host
   (concat var "="
           (mapconcat
            'identity
            (remove-if
             'null
             (mapcar
              (lambda (x)
                (when (and
                       (file-exists-p
                        (rcp-make-rcp-file-name method user host x))
                       (file-directory-p
                        (rcp-make-rcp-file-name method user host x)))
                  x))
              dirlist))
            ":")))
  (rcp-send-command method user host (concat "export " var))
  (rcp-wait-for-output))

;; -- communication with external shell -- 

;; CCC test ksh or bash found for tilde expansion?
(defun rcp-find-shell (method user host)
  "Find a shell on the remote host which groks tilde expansion."
  (let ((shell nil))
    (rcp-send-command method user host "echo ~root")
    (rcp-wait-for-output)
    (cond
     ((string-match "^~root$" (buffer-string))
      (setq shell 
            (or (rcp-find-executable method user host "ksh"  rcp-remote-path)
                (rcp-find-executable method user host "bash" rcp-remote-path)))
      (unless shell
        (error "Couldn't find a shell which groks tilde expansion."))
      (rcp-message 5 "Starting remote shell %s for tilde expansion..." shell)
      (rcp-send-command method user host (concat "exec " shell))
      (sit-for 1)                       ;why is this needed?
      (rcp-send-command method user host "unset PS1 PS2 PS3")
      (rcp-send-command method user host "echo hello")
      (rcp-message 5 "Waiting for remote %s to start up..." shell)
      (unless (rcp-wait-for-output 5)
        (pop-to-buffer (buffer-name))
        (error "Couldn't start remote %s, see buffer %s for details."
               shell (buffer-name)))
      (rcp-message 5 "Waiting for remote %s to start up...done" shell))
     (t (rcp-message 5 "Remote /bin/sh groks tilde expansion.  Good.")))))

(defun rcp-check-ls-command (method user host cmd)
  "Checks whether the given `ls' executable groks `-n'."
  (when (rcp-handle-file-executable-p
         (rcp-make-rcp-file-name method user host cmd))
    (let ((result nil))
      (rcp-message 7 "Testing remote command %s for -n..." cmd)
      (rcp-send-command
       method user host
       (format "%s -lnd / >/dev/null 2>&1 ; echo $?"
               cmd))
      (rcp-wait-for-output)
      (goto-char (point-min))
      (setq result (zerop (read (current-buffer))))
      (rcp-message 7 "Testing remote command %s for -n...%s"
                   cmd
                   (if result "okay" "failed"))
      result)))

(defun rcp-check-ls-commands (method user host cmd dirlist)
  "Checks whether the given `ls' executable in one of the dirs groks `-n'.
Returns nil if none was found, else the command is returned."
  (find nil
        (mapcar (lambda (x)
                  (when (rcp-check-ls-command
                         method user host
                         (concat (file-name-as-directory x) cmd))
                    (concat (file-name-as-directory x) cmd)))
                dirlist)
        :test-not #'equal))

(defun rcp-find-ls-command (method user host)
  "Finds an `ls' command which groks the `-n' option, returning nil if failed.
\(This option prints numeric user and group ids in a long listing.)"
  (or
   (rcp-check-ls-commands method user host "ls" rcp-remote-path)
   (rcp-check-ls-commands method user host "gnuls" rcp-remote-path)))

(defun rcp-open-connection-telnet (method user host)
  "Open a connection to HOST, logging in via telnet as USER, using METHOD."
  (rcp-pre-connection method user host)
  (let* ((telnet-program (rcp-get-telnet-program method))
         (pw (rcp-read-passwd
              (format "telnet -l %s %s -- password: " user host)))
         (p (start-process (rcp-buffer-name method user host)
                           (rcp-get-buffer method user host)
                           telnet-program host))
         (found nil)
         (i 0))
    (rcp-message 9 "Waiting for telnet login prompt...")
    (process-kill-without-query p)
    (accept-process-output p 1)
    (goto-char (point-max))
    (while (and (not (setq found (search-backward "ogin:" nil t)))
                (< i 5))
      (accept-process-output p 1)
      (goto-char (point-max))
      (incf i))
    (unless found
      (pop-to-buffer (buffer-name))
      (error "Couldn't find login prompt.  See buffer `%s' for details."
             (buffer-name)))
    (rcp-message 9 "Sending login name %s" user)
    (process-send-string nil (concat user "\n"))
    (rcp-message 9 "Waiting for password prompt...")
    (goto-char (point-max))
    (setq i 0
          found nil)
    (while (and (not (setq found (search-backward "assword:" nil t)))
                (< i 5))
      (accept-process-output p 1)
      (goto-char (point-max))
      (incf i))
    (unless found
      (pop-to-buffer (buffer-name))
      (error "Couldn't find password prompt.  See buffer `%s' for details."
             (buffer-name)))
    (rcp-message 9 "Sending password")
    (process-send-string nil (concat pw "\n"))
    (accept-process-output p 1)
    (rcp-open-connection-setup-interactive-shell p method user host)
    (rcp-post-connection method user host)))

(defun rcp-open-connection-rsh (method user host)
  "Open a connection to HOST, logging in as USER, using METHOD."
  (rcp-pre-connection method user host)
  (rcp-message 7 "Opening connection for %s@%s using %s..." user host method)
  (process-kill-without-query
   (apply #'start-process
          (rcp-buffer-name method user host)
          (rcp-get-buffer method user host) 
          (rcp-get-rsh-program method)
          (append (rcp-get-rsh-args method)
                  (list "-l" user host "/bin/sh"))))
  (rcp-message 7 "Waiting for remote /bin/sh to come up...")
  ;; Gross hack for synchronization.  How do we do this right?
  (sit-for 2)
  (rcp-send-command method user host "echo hello")
  (unless (rcp-wait-for-output 15)
    (unless (rcp-wait-for-output 5)
      (pop-to-buffer (buffer-name))
      (error "Remote /bin/sh didn't come up.  See buffer `%s' for details."
             (buffer-name))))
  (rcp-message 7 "Waiting for remote /bin/sh to come up...done")
  (rcp-post-connection method user host))

(defun rcp-open-connection-rlogin (method user host)
  "Open a connection to HOST, logging in via rlogin as USER, using METHOD."
  (rcp-pre-connection method user host)
  (let* ((pw (rcp-read-passwd
              (format "%s -l %s %s -- password: "
                      (file-name-nondirectory (rcp-get-rlogin-program method))
                      user host)))
         (p (start-process (rcp-buffer-name method user host)
                           (rcp-get-buffer method user host)
                           (rcp-get-rlogin-program method) "-l" user host))
         (found nil)
         (i 0))
    (rcp-message 9 "Waiting for rlogin passwd prompt...")
    (process-kill-without-query p)
    (accept-process-output p 1)
    (goto-char (point-max))
    (while
        (and (not (setq found
                        (re-search-backward "ass\\(word\\|phrase\\):" nil t)))
             (< i 5))
      (accept-process-output p 1)
      (goto-char (point-max))
      (incf i))
    (unless found
      (pop-to-buffer (buffer-name))
      (error "Couldn't find password prompt.  See buffer `%s' for details."
             (buffer-name)))
    (rcp-message 9 "Sending password")
    (process-send-string nil (concat pw "\n"))
    ;; Waiting twice does not seem to be the same as waiting once for
    ;; a longer time.  (Matthias Koeppe)
    (accept-process-output p 4)
    (accept-process-output p 1)
    (rcp-open-connection-setup-interactive-shell p method user host)
    (rcp-post-connection method user host)))

(defun rcp-pre-connection (method user host)
  "Do some setup before actually logging in."
  (set-buffer (rcp-get-buffer method user host))
  (set (make-local-variable 'rcp-current-method) method)
  (set (make-local-variable 'rcp-current-user)   user)
  (set (make-local-variable 'rcp-current-host)   host)
  (erase-buffer))

(defun rcp-open-connection-setup-interactive-shell
  (p method user host)
  "Set up an interactive shell such that it is ready to be used
as if it was non-interactive."
  (process-send-string nil "exec /bin/sh\n")
  (process-send-string nil "PS1=''; PS2=''; PS3=''\n")
  (accept-process-output p 1)
  (rcp-send-command method user host "stty -onlcr -echo")
  (rcp-send-command method user host "echo hello")
  (rcp-message 9 "Waiting for remote /bin/sh to come up...")
  (unless (rcp-wait-for-output 5)
    (pop-to-buffer (buffer-name))
    (error "Remote /bin/sh didn't come up.  See buffer `%s' for details."
           (buffer-name)))
  (rcp-message 7 "Waiting for remote /bin/sh to come up...done"))

(defun rcp-post-connection (method user host)
  "Prepare a remote shell before being able to work on it."
  (rcp-find-shell method user host)
  (make-local-variable 'rcp-ls-command)
  (setq rcp-ls-command (rcp-find-ls-command method user host))
  (unless rcp-ls-command
    (rcp-message
     1
     "Danger!  Couldn't find ls which groks -n.  Muddling through anyway.")
    (setq rcp-ls-command
          (rcp-find-executable method user host "ls" rcp-remote-path)))
  (rcp-message 5 "Using remote command %s for getting directory listings."
               rcp-ls-command)
  ;; Tell remote shell to use standard time format, needed for
  ;; parsing `ls -l' output.
  (rcp-send-command method user host
                    (concat "rcp_set_exit_status () {\n"
                            "return $1\n"
                            "}"))
  ;; Set remote PATH variable.
  (rcp-set-remote-path method user host "PATH" rcp-remote-path)
  (rcp-send-command method user host "LC_TIME=C; export LC_TIME; echo huhu")
  (rcp-wait-for-output))

(defun rcp-maybe-open-connection (method user host)
  "Open a connection to HOST, logging in as USER, using METHOD, if none exists."
  (let ((p (get-buffer-process (rcp-get-buffer method user host))))
    (unless (and p
                 (processp p)
                 (memq (process-status p) '(run open)))
      (when (and p (processp p))
        (delete-process p))
      (funcall (rcp-get-connection-function method)
               method user host))))

(defun rcp-send-command (method user host command &optional noerase)
  "Send the COMMAND to USER at HOST (logged in using METHOD).
Erases temporary buffer before sending the command (unless NOERASE
is true)."
  (rcp-maybe-open-connection method user host)
  (when rcp-debug-buffer
    (save-excursion
      (set-buffer (rcp-get-debug-buffer method user host))
      (goto-char (point-max))
      (insert "$ " command "\n")))
  (let ((proc nil))
    (set-buffer (rcp-get-buffer method user host))
    (unless noerase (erase-buffer))
    (setq proc (get-buffer-process (current-buffer)))
    (process-send-string proc
                         (concat command rcp-rsh-end-of-line))))

(defun rcp-wait-for-output (&optional timeout)
  "Wait for output from remote rsh command."
  (let ((proc (get-buffer-process (current-buffer)))
        (result nil))
    (process-send-string proc
                         (format "echo %s%s"
                                 rcp-end-of-output
                                 rcp-rsh-end-of-line))
    (if (not timeout)
      (while (not (setq result (looking-at (regexp-quote rcp-end-of-output))))
        (accept-process-output proc 10)
        (goto-char (point-max))
        (forward-line -1))
      (accept-process-output proc timeout)
      (goto-char (point-max))
      (forward-line -1)
      (setq result (looking-at (regexp-quote rcp-end-of-output))))
    (when result
      (delete-region (point) (progn (forward-line 1) (point)))
      (goto-char (point-min)))
    (when rcp-debug-buffer
      (append-to-buffer
       (rcp-get-debug-buffer rcp-current-method
                             rcp-current-user rcp-current-host)
       (point-min) (point-max)))
    result))

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

(defun rcp-send-region (method user host start end)
  "Send the region from START to END to remote command
running as USER on HOST using METHOD."
  (let ((proc (get-buffer-process
               (rcp-get-buffer method user host))))
    (unless proc
      (error "Can't send region to remote host -- not logged in."))
    (process-send-region proc start end)
    (when rcp-debug-buffer
      (append-to-buffer
       (rcp-get-debug-buffer method user host)
       start end))))

(defun rcp-send-eof (method user host)
  "Send EOF to the remote login as USER at HOST using METHOD."
  (let ((proc (get-buffer-process
               (rcp-get-buffer method user host))))
    (unless proc
      (error "Can't send EOF to remote host -- not logged in."))
    (process-send-eof proc)))

;; rcp file names

(defstruct rcp-file-name method user host path)

(defun rcp-rcp-file-p (name)
  "Return t iff this is an rcp file."
  (string-match rcp-file-name-regexp name))

(defun rcp-dissect-file-name (name)
  "Returns a vector: remote method, remote user, remote host, remote path name."
  ;(save-match-data
    (unless (string-match (nth 0 rcp-file-name-structure) name)
      (error "Not an rcp file name: %s" name))
    (make-rcp-file-name
     :method (or (match-string (nth 1 rcp-file-name-structure) name)
                 rcp-default-method)
     :user (or (match-string (nth 2 rcp-file-name-structure) name)
               (user-login-name))
     :host (match-string (nth 3 rcp-file-name-structure) name)
     :path (match-string (nth 4 rcp-file-name-structure) name)));)

(defun rcp-make-rcp-file-name (method user host path)
  "Constructs an rcp file name from METHOD, USER, HOST and PATH."
  (unless rcp-make-rcp-file-format
    (error "`rcp-make-rcp-file-format' is nil"))
  (rcp-substitute-percent-escapes rcp-make-rcp-file-format
                                  (list (cons "%%" "%")
                                        (cons "%m" method)
                                        (cons "%u" user)
                                        (cons "%h" host)
                                        (cons "%p" path))))

(defun rcp-make-rcp-program-file-name (user host path)
  "Creates a file name suitable to be passed to `rcp'."
  (format "%s@%s:%s" user host path))

;; Variables local to connection.

(defun rcp-get-ls-command (method user host)
  (save-excursion
    (rcp-maybe-open-connection method user host)
    (set-buffer (rcp-get-buffer method user host))
    rcp-ls-command))

(defun rcp-get-connection-function (method)
  (second (or (assoc 'rcp-connection-function
                     (assoc (or method rcp-default-method) rcp-methods))
              (list 1 rcp-connection-function))))

(defun rcp-get-rsh-program (method)
  (second (or (assoc 'rcp-rsh-program
                     (assoc (or method rcp-default-method) rcp-methods))
              (list 1 rcp-rsh-program))))

(defun rcp-get-rsh-args (method)
  (second (or (assoc 'rcp-rsh-args
                     (assoc (or method rcp-default-method) rcp-methods))
              (list 1 rcp-rsh-args))))

(defun rcp-get-rcp-program (method)
  (second (or (assoc 'rcp-rcp-program
                     (assoc (or method rcp-default-method) rcp-methods))
              (list 1 rcp-rcp-program))))

(defun rcp-get-rcp-args (method)
  (second (or (assoc 'rcp-rcp-args
                     (assoc (or method rcp-default-method) rcp-methods))
              (list 1 rcp-rcp-args))))

(defun rcp-get-rcp-keep-date-arg (method)
  (second (or (assoc 'rcp-rcp-keep-date-arg
                     (assoc (or method rcp-default-method) rcp-methods))
              (list 1 rcp-rcp-keep-date-arg))))

(defun rcp-get-encoding-command (method)
  (second (or (assoc 'rcp-encoding-command
                     (assoc (or method rcp-default-method) rcp-methods))
              (list 1 rcp-encoding-command))))

(defun rcp-get-decoding-command (method)
  (second (or (assoc 'rcp-decoding-command
                     (assoc (or method rcp-default-method) rcp-methods))
              (list 1 rcp-decoding-command))))

(defun rcp-get-encoding-function (method)
  (second (or (assoc 'rcp-encoding-function
                     (assoc (or method rcp-default-method) rcp-methods))
              (list 1 rcp-encoding-function))))

(defun rcp-get-decoding-function (method)
  (second (or (assoc 'rcp-decoding-function
                     (assoc (or method rcp-default-method) rcp-methods))
              (list 1 rcp-decoding-function))))

(defun rcp-get-rlogin-program (method)
  (second (or (assoc 'rcp-rlogin-program
                     (assoc (or method rcp-default-method) rcp-methods))
              (list 1 rcp-rlogin-program))))

(defun rcp-get-telnet-program (method)
  (second (or (assoc 'rcp-telnet-program
                     (assoc (or method rcp-default-method) rcp-methods))
              (list 1 rcp-telnet-program))))

;; general utility functions

(defun rcp-substitute-percent-escapes (str escapes)
  "Given a STRing, does percent substitution according to ESCAPES.
ESCAPES is an alist where the keys are strings of the form \"%x\" and the
values are replacement strings.  In STR, all occurrences of \"%x\" are
replaced with the given replacement string."
  (let ((m (string-match "\\([^%]*\\)\\(%.\\)\\(.*\\)" str))
        a b c)
    (if (not m)
        ;; no percent escape in string, return string unchanged
        str
      ;; first percent escape found, replace it
      (setq a (match-string 1 str))     ;left part
      (setq b (match-string 2 str))     ;middle part -- first percent escape
      (setq c (match-string 3 str))     ;right part
      ;; return value is left part plus replaced middle part
      ;; plus replacement of right part
      (concat a
              (cdr (or (assoc b escapes)
                       (error "Unknown format code: %s" b)))
              (rcp-substitute-percent-escapes c escapes)))))

;; Auto saving to a special directory.

(defun rcp-make-auto-save-name (fn)
  "Returns a file name in `rcp-auto-save-directory' for autosaving this file."
  (when rcp-auto-save-directory
    (unless (file-exists-p rcp-auto-save-directory)
      (make-directory rcp-auto-save-directory t))
    (expand-file-name
     (rcp-subst-char-in-string ?/ ?| fn)
     rcp-auto-save-directory)))

;; ------------------------------------------------------------ 
;; -- Compatibility functions section -- 
;; ------------------------------------------------------------ 

(eval-when-compile
  (unless (fboundp 'subst-char-in-string)
    (fset 'subst-char-in-string 'ignore)))

(defun rcp-subst-char-in-string (from to string)
  "Replace all occurrences of the character FROM with TO in STRING."
  (if (fboundp 'subst-char-in-string)
      (subst-char-in-string from to string)
    (while (string-match (regexp-quote (char-to-string from)) string)
      (setq string (replace-match (char-to-string to) t t string)))
    string))

(eval-when-compile
  (when (eq (symbol-function 'subst-char-in-string) 'ignore)
    (fmakunbound 'subst-char-in-string)))

(defun rcp-temporary-file-directory ()
  "Returns name of directory for temporary files (compat function).
For Emacs, this is the variable `temporary-file-directory', for XEmacs
this is the function `temp-directory'."
  (cond ((boundp 'temporary-file-directory) temporary-file-directory)
        ((fboundp 'temp-directory) (temp-directory))
        (t (message (concat "Neither `temporary-file-directory' nor "
                            "`temp-directory' is defined -- using /tmp."))
           (file-name-as-directory "/tmp"))))

(defun rcp-read-passwd (prompt)
  "Read a password from user (compat function).
Invokes `read-passwd' if that is defined, else `ange-ftp-read-passwd'."
  (apply
   (if (fboundp 'read-passwd) #'read-passwd #'ange-ftp-read-passwd)
   (list prompt)))

;; Daniel Pittman: EFS hooks itself into the file name handling stuff
;; in more places than just `file-name-handler-alist'.  The following
;; tells EFS to stay away from rcp.el paths.
;; (Exactly where does EFS hook itself into things? -- kai)
(defadvice efs-ftp-path (around dont-match-rcp-path activate protect)
  "Cause efs-ftp-path to fail when the path is an RCP path."
  (if (rcp-rcp-file-p (ad-get-arg 0))
      nil
    ad-do-it))

;;; TODO:

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
;; * How to deal with MULE in `insert-file-contents' and `write-region'?
;; * Do asynchronous `shell-command's.
;; * Grok `append' and `lockname' parameters for `write-region'.
;; * Test remote ksh or bash for tilde expansion in `rcp-find-shell'?
;; * vc-user-login-name: whenever it is called from VC, the variable
;;   `file' is bound to the current file name.  Thus, we can change
;;   vc-user-login-name such that it does the right thing with rcp.el
;;   files.
;;   Find out who called me:
;;   (defun foo ()
;;     (let ((caller (backtrace-frame 3)))
;;       (message "%s" (symbol-name (cadr caller)))))
;; * abbreviate-file-name
;; * file name completion doesn't work for /r:user@host:<TAB>?
;;   (Henrik Holm <henrikh@tele.ntnu.no>)
;; * grok ~ in rcp-remote-path  (Henrik Holm <henrikh@tele.ntnu.no>)
;; * `C' in dired gives error `not rcp file name'.
;; * instead of putting in user-login-name as remote login, rely
;;   on ssh/scp to fill these in.  Make this controllable with a variable.
;; * new method using `su' to edit files on local host as different user
;;   suggestion by Greg Stark <gsstark@mit.edu>
;; * better error checking.  At least whenever we see something
;;   strange when doing zerop, we should kill the process and start
;;   again.  (Greg Stark)
;; * Add caching for filename completion.  (Greg Stark)
;; * Provide a local cache of old versions of remote files for the rsync
;;   transfer method to use.  (Greg Stark)
;; * Do not require the user to know beforehand whether a particular
;;   connection attempt requires passwd entry.  (Greg Stark)
;;   Maybe support passwd entry for scp?
;; * Remove unneeded parameters from methods.
;; * Invoke rsync once for copying a whole directory hierarchy.
;;   (Francesco PotortÅÏ)
;; * Maybe extract remote environment from shell startup scripts: instead
;;   of "rsh -l USER HOST /bin/sh", say "rsh -l USER HOST", then wait
;;   a bit, then say "exec /bin/sh".

;; Functions for file-name-handler-alist:
;; diff-latest-backup-file -- in diff.el
;; directory-file-name -- use primitive?
;; dired-compress-file
;; dired-uncache -- this will be needed when we do insert-directory caching
;; file-modes
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
;; rename-file
;; set-file-modes
;; set-visited-file-modtime
;; shell-command
;; unhandled-file-name-directory
;; vc-registered
;; verify-visited-file-modtime

;;; rcp.el ends here
