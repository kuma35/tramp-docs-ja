;;; rcp.el --- remote file editing using rsh/rcp or similar programs

;; Copyright (C) 1998 by Kai Grossjohann

;; Author: Kai.Grossjohann@CS.Uni-Dortmund.DE
;; Keywords: comm, processes
;; Version: $Id: tramp.el,v 1.55 1999/03/05 12:55:24 grossjoh Exp $

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
;; of rsh and rcp or other work-alike programs.
;;
;; Installation is simple -- it is sufficient to load this file.  EFS
;; users should do (require 'efs) before loading this file, though.
;; This is such that the regexes for rcp files come before the
;; regexes for EFS files in `file-name-handler-alist'.
;;
;; Usage is also simple: it's just like ange-ftp, but uses a different
;; syntax for the remote file names.  The syntax used is as follows:
;;
;; /r:USER@HOST:FILENAME
;;
;; This logs you in as USER to the remote HOST, retrieving FILENAME.
;; The "USER@" part can be omitted, in this case the current local
;; user name is used.
;;
;; WARNING!
;;
;; This file is in a very early stage of development, there are a
;; number of limitations and of course a lot of unknown bugs.
;;
;; Firstly, you MUST have set up rsh such that you can log in as USER
;; to the remote HOST without entering a password or pass phrase.  The
;; code does *not* check if you have done this, so expect strange
;; effects when rsh asks you for a password or pass phrase.  (It is
;; fairly easy to allow login without entering a password or pass
;; phrase by using .rhosts files.  Other work-alike programs might
;; offer other methods.  Thus, I did not deem it important to deal
;; with querying for passwords in this early stage of development.)
;;
;; Secondly, there is almost no error checking at all.
;;
;; I have thus far tested this on one machine, as one user, with two
;; files and one directory.
;;
;; Here's what seems to work so far:
;;   - opening and saving files
;;   - dired
;;   - filename completion
;;
;; Known problems:
;;   - BSD ls doesn't grok `-n' option for printing numeric user and
;;     group ids.  Use `gnuls' instead.
;;
;; Also see the todo list at the bottom of this file.
;;
;; There's a mailing list for this, as well.  Its name is:
;;                emacs-rcp@ls6.cs.uni-dortmund.de
;; Send a mail with `help' in the subject (!) to the administration
;; address for instructions on joining the list.  The administration
;; address is:
;;            emacs-rcp-request@ls6.cs.uni-dortmund.de
;; You may also mail me, Kai, directly.

;;; Code:

(require 'cl)
(require 'comint)
;; Emacs 19.34 compatibility hack -- is this needed?
(or (>= emacs-major-version 20)
    (load "cl-seq"))

(provide 'rcp)

;;; User Customizable Internal Variables:

(defvar rcp-verbose 3
  "*Verbosity level for rcp.el.  0 means be silent, 10 is most verbose.")

(defvar rcp-file-name-quote-list
  '(?] ?[ ?\| ?& ?< ?> ?\( ?\) ?\; ?\  ?\* ?\? ?\! ?\" ?\' ?\` ?# ?\@ ?\+ )
  "*Protect these characters from the remote shell.
Any character in this list is quoted (preceded with a backslash)
because it means something special to the shell.  This takes effect
when sending file and directory names to the remote shell.

See `comint-file-name-quote-list' for details.")

(defvar rcp-rsh-program "rsh"
  "*Name of rsh program.")

(defvar rcp-rsh-args nil
  "*Args for running rsh.")

(defvar rcp-rcp-program "rcp"
  "*Name of rcp program.")

(defvar rcp-rsh-end-of-line "\n"
  "*String used for end of line in rsh connections.")

(defvar rcp-remote-path '("/bin" "/usr/bin" "/usr/sbin")
  "*List of directories to search for executables on remote host.")

(defvar rcp-ls-command-alist
  '(("" . "ls"))
  "*Alist saying what command is used to invoke `ls' for each host.
The key is a regex matched against the host name, the value is the
name to use for `ls'.")

(defvar rcp-cd-command-alist
  '(("" . "cd"))
  "*Alist saying what command is used to invoke `cd' for each host.
The key is a regex matched against the host name, the value is the
name to use for `cd'.")

(defvar rcp-test-command-alist
  '(("" . "test"))
  "*Alist saying what command is used to invoke `test' for each host.
The key is a regex matched against the host name, the value is the
name to use for `test'.")

(defvar rcp-mkdir-command-alist
  '(("" . "mkdir"))
  "*Alist saying what command is used to invoke `mkdir' for each host.
The key is a regex matched against the host name, the value is the
name to use for `mkdir'.
This does not need to create parent directories.")

(defvar rcp-mkdir-p-command-alist
  '(("" . "mkdir -p"))
  "*Alist saying what command is used to invoke `mkdir -p' for each host.
The key is a regex matched against the host name, the value is the
name to use for `mkdir -p'.
`mkdir -p' should create parent directories which do not exist.")

(defvar rcp-rmdir-command-alist
  '(("" . "rmdir"))
  "*Alist saying what command is used to invoke `rmdir' for each host.
The key is a regex matched against the host name, the value is the
name to use for `rmdir'.")

(defvar rcp-rm-f-command-alist
  '(("" . "rm -f"))
  "*Alist saying what command is used to invoke `rm -f' for each host.
The key is a regex matched against the host name, the value is the
name to use for `rm -f'.
This command should produce as little output as possible, hence `-f'.")

;; File name format.

(defvar rcp-file-name-structure
  (list "\\`/r:\\(\\([a-z0-9_]+\\)@\\)?\\([a-z0-9.-]+\\):\\(.*\\)\\'"
        2 3 4)
  "*List of four elements, detailing the rcp file name structure.

The first element is a regular expression matching an rcp file name.
The regex should contain parentheses around the user name, the host
name, and the file name parts.

The second element is a number, saying which pair of parentheses matches
the user name.  The third element is similar, but for the host name.
The fourth element is for the file name.  These numbers are passed
directly to `match-string', which see.  That means the opening parentheses
are counted to identify the pair.

See also `rcp-file-name-regexp' and `rcp-make-rcp-file-format'.")

(defvar rcp-file-name-regexp "\\`/r:"
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

Also see `rcp-file-name-structure' and `rcp-make-rcp-file-format'.")

(defvar rcp-make-rcp-file-format "/r:%u@%h:%p"
  "*Format string saying how to construct rcp file name.
%u is replaced by user name.
%h is replaced by host name.
%p is replaced by file name.
%% is replaced by %.

Also see `rcp-file-name-structure' and `rcp-file-name-regexp'.")

;;; Internal Variables:

(defvar rcp-end-of-output "/////"
  "String used to recognize end of output.")

;; New handlers should be added here.
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
    (dired-call-process . rcp-handle-dired-call-process)
    (shell-command . rcp-handle-shell-command)
    (insert-directory . rcp-handle-insert-directory)
    (expand-file-name . rcp-handle-expand-file-name)
    (file-local-copy . rcp-handle-file-local-copy)
    (insert-file-contents . rcp-handle-insert-file-contents)
    (write-region . rcp-handle-write-region))
        "Alist of handler functions.
Operations not mentioned here will be handled by the normal Emacs functions.")

;;; Internal functions which must come first.

(defsubst rcp-message (level fmt-string &rest args)
  (when (>= level rcp-verbose)
    (apply #'message fmt-string args)))

;; Extract right value of alists, depending on host name.

(defsubst rcp-alist-get (string alist)
  "Return the value from the alist, based on regex matching against the keys."
  (cdr (assoc* string alist
               :test (function (lambda (a b)
                                 (string-match b a))))))

(defsubst rcp-ls-command-get (host)
  "Return the `ls' command name for HOST.  See `rcp-ls-command-alist'."
  (rcp-alist-get host rcp-ls-command-alist))

(defsubst rcp-ls-command (host switches file)
  "Return `ls' command for HOST with SWITCHES on FILE.
SWITCHES is a string."
  (let ((comint-file-name-quote-list rcp-file-name-quote-list))
    (format "%s %s %s"
            (rcp-ls-command-get host)
            switches
            (comint-quote-filename file))))

(defsubst rcp-cd-command-get (host)
  "Return the `cd' command name for HOST.  See `rcp-cd-command-alist'."
  (rcp-alist-get host rcp-cd-command-alist))

(defsubst rcp-cd-command (host dir)
  "Return the `cd' command for HOST with DIR."
  (let ((comint-file-name-quote-list rcp-file-name-quote-list))
    (format "%s %s" (rcp-cd-command-get host)
            (comint-quote-filename dir))))

(defsubst rcp-test-command-get (host)
  "Return the `test' command name for HOST.  See `rcp-test-command-alist'."
  (rcp-alist-get host rcp-test-command-alist))

(defsubst rcp-mkdir-command-get (host)
  "Return the `mkdir' command name for HOST.  See `rcp-mkdir-command-alist'."
  (rcp-alist-get host rcp-mkdir-command-alist))

(defsubst rcp-mkdir-p-command-get (host)
  "Return the `mkdir -p' command name for HOST.  See `rcp-mkdir-p-command-alist'."
  (rcp-alist-get host rcp-mkdir-p-command-alist))

(defsubst rcp-rmdir-command-get (host)
  "Return the `rmdir' command name for HOST.  See `rcp-rmdir-command-alist'."
  (rcp-alist-get host rcp-rmdir-command-alist))

(defsubst rcp-rmdir-command (host dir)
  "Return the `rmdir' command for HOST with DIR."
  (let ((comint-file-name-quote-list rcp-file-name-quote-list))
    (format "%s %s" (rcp-rmdir-command-get host)
            (comint-quote-filename dir))))

(defsubst rcp-rm-f-command-get (host)
  "Return the `rm -f' command name for HOST.  See `rcp-rm-f-command-alist'."
  (rcp-alist-get host rcp-rm-f-command-alist))

;;; File Name Handler Functions:

;; Basic functions.

(defun rcp-handle-file-exists-p (filename)
  "Like `file-exists-p' for rcp files."
  (let ((v (rcp-dissect-file-name filename))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        user host path)
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (rcp-send-command user host
                         (format "%s -d %s >/dev/null 2>&1"
                                 (rcp-ls-command-get host)
                                 (comint-quote-filename path)))
      (rcp-send-command user host "echo $?")
      (rcp-wait-for-output)
      (zerop (read (current-buffer))))))

(defun rcp-handle-file-attributes (filename)
  "Like `file-attributes' for rcp files."
  (let ((v (rcp-dissect-file-name filename))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        user host path symlinkp dirp
        res-inode res-filemodes res-numlinks
        res-uid res-gid res-size res-symlink-target)
    (if (not (rcp-handle-file-exists-p filename))
        nil                             ; file cannot be opened
      ;; file exists, find out stuff
      (save-excursion
        (rcp-send-command
         (rcp-file-name-user v) (rcp-file-name-host v)
         (format "%s -iLldn %s"
                 (rcp-ls-command-get (rcp-file-name-host v))
                 (comint-quote-filename (rcp-file-name-path v))))
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
    

;; Directory listings.

(defun rcp-handle-directory-files (directory &optional full match nosort)
  "Like `directory-files' for rcp files."
  (let ((v (rcp-dissect-file-name directory))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        user host path result x)
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (if full
          (rcp-send-command
           user host (format "%s -ad %s" (rcp-ls-command-get host)
                             (comint-quote-filename path)))
        (rcp-send-command user host (rcp-cd-command host path))
        (rcp-send-command
         user host
         (format "%s -a" (comint-quote-filename (rcp-ls-command-get host)))))
      (rcp-wait-for-output)
      (goto-char (point-max))
      (while (zerop (forward-line -1))
        (setq x (buffer-substring (point)
                                  (progn (end-of-line) (point))))
        (if match
            (when (string-match match x) (push x result))
          (push x result))))
    result))

(defun rcp-handle-file-name-all-completions (file directory)
  "Like `file-name-all-completions' for rcp files."
  (let ((v (rcp-dissect-file-name directory))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        user host path result)
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (rcp-send-command user host (rcp-cd-command host path))
      (rcp-send-command user host
                         (format "%s -ad %s* 2>/dev/null"
                                 (rcp-ls-command-get host)
                                 (comint-quote-filename file)))
      (rcp-wait-for-output)
      (goto-char (point-max))
      (while (zerop (forward-line -1))
        (push (buffer-substring (point)
                                (progn (end-of-line) (point)))
              result)))
    ;; Now go through the list of file names and add a slash to all
    ;; directories.  We don't use `ls -p' because that option appears
    ;; to be nonstandard.  We don't use `ls -F' because that option
    ;; adds suffixes for other kinds of files, too (such as `@' for a
    ;; symlink), and we cannot tell whether these are part of the file
    ;; name or were added by `ls -F'.
    (mapcar
     (function (lambda (x)
                 (if (rcp-handle-file-directory-p
                      (concat (file-name-as-directory directory) x))
                     (file-name-as-directory x)
                   x)))
     result)))

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
  (error "add-name-to-file not implemented yet for rcp files."))

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
  (let ((v1 (when (rcp-rcp-file-p file)
              (rcp-dissect-file-name file)))
        (v2 (when (rcp-rcp-file-p newname)
              (rcp-dissect-file-name newname))))
    (let ((f1 (if (not v1)
                  file
                (format "%s@%s:%s"
                        (rcp-file-name-user v1)
                        (rcp-file-name-host v1)
                        (comint-quote-filename (rcp-file-name-path v1)))))
          (f2 (if (not v2)
                  newname
                (format "%s@%s:%s"
                        (rcp-file-name-user v2)
                        (rcp-file-name-host v2)
                        (comint-quote-filename (rcp-file-name-path v2))))))
      (if keep-date
          (call-process rcp-rcp-program nil nil nil "-p" f1 f2)
        (call-process rcp-rcp-program nil nil nil f1 f2)))))

;; mkdir
(defun rcp-handle-make-directory (dir &optional parents)
  "Like `make-directory' for rcp files."
  (let ((v (rcp-dissect-file-name dir))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        host)
    (setq host (rcp-file-name-host v))
    (rcp-send-command
     (rcp-file-name-user v) host
     (format "%s %s"
             (if parents
                 (rcp-mkdir-p-command-get host)
               (rcp-mkdir-command-get host))
             (comint-quote-filename (rcp-file-name-path v))))))

;; error checking?
(defun rcp-handle-delete-directory (directory)
  "Like `delete-directory' for rcp files."
  (let ((v (rcp-dissect-file-name directory))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        host result)
    (setq host (rcp-file-name-host v))
    (save-excursion
      (rcp-send-command
       (rcp-file-name-user v) host
       (format "%s %s ; echo ok"
               (rcp-rmdir-command-get host)
               (comint-quote-filename (rcp-file-name-path v))))
      (rcp-wait-for-output))))

(defun rcp-handle-delete-file (filename)
  "Like `delete-file' for rcp files."
  (let ((v (rcp-dissect-file-name filename))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        host result)
    (setq host (rcp-file-name-host v))
    (save-excursion
      (rcp-send-command
       (rcp-file-name-user v)
       (rcp-file-name-host v)
       (format "%s %s ; echo ok"
               (rcp-rm-f-command-get host)
               (comint-quote-filename (rcp-file-name-path v))))
      (rcp-wait-for-output))))

;; Dired.

(defun rcp-handle-dired-call-process (program discard &rest arguments)
  "Like `dired-call-process' for rcp files."
  (let ((v (rcp-dissect-file-name default-directory))
        user host path result)
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (rcp-send-command user host path)
      (rcp-send-command user host
                         (mapconcat #'identity (cons program arguments)))
      (rcp-wait-for-output))
    (unless discard
      (insert-buffer (rcp-get-buffer user host)))
    (save-excursion
      (rcp-send-command user host "echo $?")
      (rcp-wait-for-output)
      (read (rcp-get-buffer user host)))))

(defun rcp-handle-insert-directory
  (file switches &optional wildcard full-directory-p)
  "Like `insert-directory' for rcp files."
  (let ((v (rcp-dissect-file-name file))
        user host path)
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (when (listp switches)
      (setq switches (mapconcat #'identity switches " ")))
    (unless full-directory-p
      (setq switches (concat "-d " switches)))
    (save-excursion
      (rcp-send-command user host
                         (rcp-cd-command host path))
      (rcp-send-command user host
                         (rcp-ls-command host switches ""))
      (sit-for 1)                       ;needed for rsh but not ssh?
      (rcp-wait-for-output))
    (insert-buffer (rcp-get-buffer user host))))

;; Canonicalization of file names.

(defun rcp-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for rcp files."
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
           (user (rcp-file-name-user v))
           (host (rcp-file-name-host v))
           (path (rcp-file-name-path v)))
      (unless (file-name-absolute-p path)
        (setq path (concat "~/" path)))
      (save-excursion
        ;; Tilde expansion if necessary.  This needs a shell
        ;; which groks tilde expansion!  Maybe you need to set
        ;; rcp-sh-command-alist to /usr/bin/ksh for some hosts
        ;; where sh is too stupid?
        (when (string-match "\\`\\(~[^/]*\\)\\(.*\\)\\'" path)
          (let ((uname (match-string 1 path))
                (fname (match-string 2 path)))
            (rcp-send-command
             user host
             (format "cd %s; pwd" uname))
            (rcp-wait-for-output)
            (goto-char (point-min))
            (setq uname (buffer-substring (point)
                                          (progn (end-of-line)
                                                 (point))))
            (setq path (concat uname fname))))
        ;; No tilde characters in file name, do normal
        ;; expand-file-name (this does "/./" and "/../").
        (rcp-make-rcp-file-name
         user host
         (rcp-run-real-handler 'expand-file-name (list path)))))))

;; Remote commands.

(defun rcp-handle-shell-command (command &optional output-buffer)
  "Like `shell-command' for rcp files.
Bug: COMMAND must not output the string `/////'.
Bug: output of COMMAND must end with a newline."
  (if (rcp-rcp-file-p default-directory)
      (let* ((v (rcp-dissect-file-name default-directory))
             (comint-file-name-quote-list rcp-file-name-quote-list)
             (user (rcp-file-name-user v))
             (host (rcp-file-name-host v))
             (path (rcp-file-name-path v)))
        (when (string-match "&[ \t]*\\'" command)
          (error "Rcp doesn't grok asynchronous shell commands, yet."))
        (save-excursion
          (rcp-send-command
           user host (format "cd %s; pwd" (comint-quote-filename path)))
          (rcp-wait-for-output)
          (rcp-send-command user host command)
          ;; This will break if the shell command prints "/////"
          ;; somewhere.  Let's just hope for the best...
          (rcp-wait-for-output))
        (unless output-buffer
          (setq output-buffer (get-buffer-create "*Shell Command Output*")))
        (unless (bufferp output-buffer)
          (setq output-buffer (current-buffer)))
        (set-buffer output-buffer)
        (erase-buffer)
        (insert-buffer (rcp-get-buffer user host))
        (unless (zerop (buffer-size))
          (pop-to-buffer output-buffer)))
    ;; The following is only executed if something strange was
    ;; happening.  Emit a helpful message and do it anyway.
    (message "rcp-handle-shell-command called with non-rcp directory: %s"
             default-directory)
    (rcp-run-real-handler 'shell-command
                           (list command output-buffer))))

;; File Editing.

(defun rcp-handle-file-local-copy (file)
  "Like `file-local-copy' for rcp files."
  (let ((v (rcp-dissect-file-name filename))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        tmpfil)
    (setq tmpfil (make-temp-name "/tmp/rcp."))
    (rcp-message 5 "Fetching %s to tmp file %s..." file tmpfil)
    (call-process rcp-rcp-program nil nil nil
                  (format "%s@%s:%s"
                          (rcp-file-name-user v)
                          (rcp-file-name-host v)
                          (comint-quote-filename (rcp-file-name-path v)))
                  tmpfil)
    (rcp-message 5 "Fetching %s to tmp file %s...done" file tmpfil)
    tmpfil))

;; CCC need to do MULE stuff
(defun rcp-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for rcp files."
  (let ((local-copy (rcp-handle-file-local-copy filename)))
    (when visit
      (setq buffer-file-name filename)
      (set-visited-file-modtime '(0 0))
      (set-buffer-modified-p nil)
      ;; Is this the right way to go about auto-saving?
      (when auto-save-default (auto-save-mode 1)))
    (rcp-run-real-handler 'insert-file-contents
                           (list local-copy nil beg end replace))
    (delete-file local-copy)
    ))

(defun rcp-handle-write-region
  (start end filename &optional append visit lockname confirm)
  "Like `write-region' for rcp files."
  (unless (eq append nil)
    (error "rcp-handle-write-region: APPEND must be nil."))
  (unless (or (eq lockname nil)
              (string= lockname filename))
    (error "rcp-handle-write-region: LOCKNAME must be nil or equal FILENAME."))
  (unless (eq confirm nil)
    (error "rcp-handle-write-region; CONFIRM must be nil."))
  (let ((v (rcp-dissect-file-name filename))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        tmpfil)
    (setq tmpfil (make-temp-name "/tmp/rcp."))
    (rcp-run-real-handler
     'write-region
     (if confirm ; don't pass this arg unless defined for backward compat.
         (list start end tmpfil append 'no-message lockname confirm)
       (list start end tmpfil append 'no-message lockname)))
    (call-process rcp-rcp-program nil nil nil
                  tmpfil
                  (format "%s@%s:%s"
                          (rcp-file-name-user v)
                          (rcp-file-name-host v)
                          (comint-quote-filename (rcp-file-name-path v))))
    (delete-file tmpfil)
    (when visit
      ;; Is this right for auto-saving?
      (when auto-save-default (auto-save-mode 1)))
    (when (or (eq visit t)
              (eq visit nil)
              (stringp visit))
      (message "Wrote %s" filename))))

;; Main function.
(defun rcp-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers
	 (cons 'rcp-file-name-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

(defun rcp-file-name-handler (operation &rest args)
  (let ((fn (assoc operation rcp-file-name-handler-alist)))
    (if fn
        (apply (cdr fn) args)
      (rcp-run-real-handler operation args))))

;; Register in file name handler alist

(add-to-list 'file-name-handler-alist
             (cons rcp-file-name-regexp 'rcp-file-name-handler))

;;; Interactions with other packages:

;; -- complete.el --

;; This function contributed by Ed Sabol
(defun rcp-handle-expand-many-files (name)
  "Like `PC-expand-many-files' for rcp files."
  (if (or (string-match "\\*" name)
          (string-match "\\?" name)
          (string-match "\\[.*\\]" name))
      ;; Dissect NAME.
      (let* ((v (rcp-dissect-file-name name))
             (user (rcp-file-name-user v))
             (host (rcp-file-name-host v))
             (path (rcp-file-name-path v))
             (comint-file-name-quote-list rcp-file-name-quote-list)
             bufstr)
        (let ((comint-file-name-quote-list
               (set-difference rcp-file-name-quote-list '(?\* ?\? ?[ ?]))))
          (rcp-send-command user host (format "echo %s"
                                              (comint-quote-filename path)))
          (rcp-wait-for-output))
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
                       (rcp-make-rcp-file-name user host x)))
           (read (current-buffer)))))
    (list (rcp-handle-expand-file-name name))))

;; Check for complete.el and override PC-expand-many-files if appropriate.
(defun rcp-setup-complete ()
  (defun rcp-save-PC-expand-many-files (name)); avoid compiler warning
  (fset 'rcp-save-PC-expand-many-files
        (symbol-function 'PC-expand-many-files))
  (defun PC-expand-many-files (name)
    (if (rcp-rcp-file-p name)
        (rcp-handle-expand-many-files name)
      (rcp-save-PC-expand-many-files name))))
;; CCC: Is the following really needed?
(if (fboundp 'PC-expand-many-files)
    (rcp-setup-complete)
  (eval-after-load "complete" '(rcp-setup-complete)))


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
        (comint-file-name-quote-list rcp-file-name-quote-list)
        host result)
    (setq host (rcp-file-name-host v))
    (save-excursion
      (rcp-send-command
       (rcp-file-name-user v) host
       (format "%s %s %s ; echo $?"
               (rcp-test-command-get host) switch
               (comint-quote-filename (rcp-file-name-path v))))
      (rcp-wait-for-output)
      (read (current-buffer)))))

(defun rcp-buffer-name (user host)
  "A name for the connection buffer for USER at HOST."
  (format "*rcp %s@%s*" user host))

(defun rcp-get-buffer (user host)
  "Get the connection buffer to be used for USER at HOST."
  (get-buffer-create (rcp-buffer-name user host)))

(defun rcp-find-executable (user host progname dirlist)
  "Searches for PROGNAME in all directories mentioned in `rcp-remote-path'.
This one expects to be in the right *rcp* buffer."
  (let (result x)
    (while (and (null result) dirlist)
      (setq x (concat (file-name-as-directory (pop dirlist)) progname))
      (rcp-message 5 "Looking for remote executable %s" x)
      (when (rcp-handle-file-executable-p
             (rcp-make-rcp-file-name user host x))
        (setq result x)))
    (rcp-message 5 "Found remote executable %s" result)
    result))

;; -- communication with external shell -- 

(defun rcp-find-shell (user host)
  "Find a shell on the remote host which groks tilde expansion."
  (let ((shell nil))
    (rcp-send-command user host "echo ~root")
    (rcp-wait-for-output)
    (unless (string-equal (buffer-string) "/\n")
      (setq shell 
            (or (rcp-find-executable user host "ksh" rcp-remote-path)
                (rcp-find-executable user host "bash" rcp-remote-path)))
      (unless shell
        (error "Couldn't find a shell which groks tilde expansion."))
      (rcp-message 5 "Starting remote shell %s for tilde expansion..." shell)
      (rcp-send-command user host (concat "exec " shell))
      (sit-for 1)                       ;why is this needed?
      (rcp-send-command user host "echo hello")
      (rcp-message 5 "Waiting for remote %s to start up..." shell)
      (rcp-wait-for-output)
      (rcp-message 5 "Waiting for remote %s to start up...done" shell))))

(defun rcp-open-connection-rsh (user host)
  "Open a connection to HOST, logging in as USER, using rsh."
  (set-buffer (rcp-get-buffer user host))
  (erase-buffer)
  (apply #'start-process
         (rcp-buffer-name user host)
         (rcp-get-buffer user host) 
         rcp-rsh-program
         (append rcp-rsh-args
                 (list "-l" user host "/bin/sh")))
  ;; Gross hack for synchronization.  How do we do this right?
  (rcp-send-command user host "echo hello")
  (rcp-wait-for-output)
  (rcp-find-shell user host))

(defun rcp-maybe-open-connection-rsh (user host)
  "Open a connection to HOST, logging in as USER, using rsh, if none exists."
  (let ((p (get-buffer-process (rcp-get-buffer user host))))
    (unless (and p
                 (processp p)
                 (memq (process-status p) '(run open)))
      (rcp-open-connection-rsh user host))))

(defun rcp-send-command (user host command)
  "Send the COMMAND to USER at HOST."
  (rcp-maybe-open-connection-rsh user host)
  (let ((proc nil))
    (set-buffer (rcp-get-buffer user host))
    (erase-buffer)
    (setq proc (get-buffer-process (current-buffer)))
    (process-send-string proc
                         (concat command rcp-rsh-end-of-line))))

(defun rcp-wait-for-output ()
  "Wait for output from remote rsh command."
  (let ((proc (get-buffer-process (current-buffer))))
    (process-send-string proc
                         (format "echo %s%s"
                                 rcp-end-of-output
                                 rcp-rsh-end-of-line))
    (while (progn (goto-char (point-max))
                  (forward-line -1)
                  (not (looking-at (regexp-quote rcp-end-of-output))))
      (accept-process-output proc))
    (delete-region (point) (progn (forward-line 1) (point)))
    (goto-char (point-min))))

;; rcp file names

(defstruct rcp-file-name user host path)

(defun rcp-rcp-file-p (name)
  "Return t iff this is an rcp file."
  (string-match rcp-file-name-regexp name))

(defun rcp-dissect-file-name (name)
  "Returns a vector: remote user, remote host, remote path name."
  (unless (string-match (nth 0 rcp-file-name-structure) name)
    (error "Not an rcp file name: %s" name))
  (make-rcp-file-name
   :user (or (match-string (nth 1 rcp-file-name-structure)
                           name)
             (user-login-name))
   :host (match-string (nth 2 rcp-file-name-structure)
                       name)
   :path (match-string (nth 3 rcp-file-name-structure)
                       name)))

;; CCC: This must be changed for other rcp file name formats!
(defun rcp-make-rcp-file-name (user host path &optional str)
  "Constructs an rcp file name from USER, HOST and PATH."
  (let ((fmt-alist (list (cons "%%" "%")
                         (cons "%u" user)
                         (cons "%h" host)
                         (cons "%p" path)))
        (m (string-match "\\([^%]*\\)\\(%.\\)\\(.*\\)"
                         (or str rcp-make-rcp-file-format)))
        a b c x)
    (unless rcp-make-rcp-file-format
      (error "`rcp-make-rcp-file-format' is nil"))
    (if (not m)
        ;; return accumulated string if not match
        (or str
            (error "rcp-make-rcp-file-format doesn't contain % escapes"))
      (setq x (or str rcp-make-rcp-file-format))
      (setq a (match-string 1 x))
      (setq b (match-string 2 x))
      (setq c (match-string 3 x))
      (concat a
              (cdr (or (assoc b fmt-alist)
                       (error "Unknown format code: %s" b)))
              (rcp-make-rcp-file-name user host path c)))))


;;; TODO:

;; * Make rcp-handle-file-name-all-completions faster.
;; * Use more variables for program names.
;; * Make it possible to make program names dependent on system type
;;   as well as host name.
;; * Is it cleaner to use whole commands rather than just abbrevs for
;;   the binary?
;; * BSD doesn't grok `-n' to print numeric user/group ids.
;; * ``Active processes exist; kill them and exit anyway?''
;; * Make sure permissions of tmp file are good.
;;   (Nelson Minar <nelson@media.mit.edu>)
;; * Temporary directory should be customizable.
;;   (Francesco PotortÅÏ <F.Potorti@cnuce.cnr.it>)
;; * Do copy-file.  Does it work to always use rcp?
;; * Use timeout for starting the remote shell.
;; * Automatically see whether remote /bin/sh groks tilde expansion,
;;   look for ksh if it doesn't.
;; * Automatically find out as much as possible about the remote system.
;;   Maybe it would be best to have a list of directories to search for
;;   executables on all systems, and a list of program names to try?
;;   (E.g. one could try `gnuls' on BSD systems for the numeric user id
;;   thing.)
;;   Francesco suggests that one could have variables which determine
;;   how to do things on remote systems, if there is no match for
;;   the remote system name, rcp.el should guess.
;; * Util function for creating an rsh/rcp file name argument.
;; * Dired header line contains duplicated directory name.
;; * Use rsync if available.  Fall back to rcp if scp isn't available.
;;   (Francesco PotortÅÏ <F.Potorti@cnuce.cnr.it>)
;; * rcp program name should be customizable on per-host basis?
;;   (Francesco PotortÅÏ <F.Potorti@cnuce.cnr.it>)
;; * Grok passwd prompts.  (David Winter <winter@nevis1.nevis.columbia.edu>)
;;   Maybe just do `rsh -l user host', then wait a while for the passwd
;;   or passphrase prompt.  If there is one, remember the passwd/phrase.

;; Functions for file-name-handler-alist:
;; diff-latest-backup-file -- in diff.el
;; directory-file-name -- use primitive?
;; dired-compress-file
;; dired-uncache -- this will be needed when we do insert-directory caching
;; file-modes
;; file-name-as-directory -- use primitive?
;; file-name-completion -- not needed?  completion seems to work okay
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
