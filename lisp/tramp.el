;;; rcp.el --- remote file editing using rsh/rcp or work-alike programs

;; Copyright (C) 1998, 1999 Free Software Foundation, Inc.

;; Author: Kai.Grossjohann@CS.Uni-Dortmund.DE
;; Keywords: comm, processes
;; Version: $Id: tramp.el,v 1.77 1999/04/19 16:01:28 grossjoh Exp $

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

(require 'cl)
(require 'comint)
(require 'vc)                           ;for doing remote vc
(require 'timezone)
;; Emacs 19.34 compatibility hack -- is this needed?
(or (>= emacs-major-version 20)
    (load "cl-seq"))

(provide 'rcp)

;;; User Customizable Internal Variables:

(defgroup rcp nil
  "Edit remote files with a combination of rsh and rcp or similar programs."
  :group 'files)

(defcustom rcp-verbose 3
  "*Verbosity level for rcp.el.  0 means be silent, 10 is most verbose."
  :group 'rcp
  :type 'integer)

(defcustom rcp-file-name-quote-list
  '(?] ?[ ?\| ?& ?< ?> ?\( ?\) ?\; ?\  ?\* ?\? ?\! ?\" ?\' ?\` ?# ?\@ ?\+ )
  "*Protect these characters from the remote shell.
Any character in this list is quoted (preceded with a backslash)
because it means something special to the shell.  This takes effect
when sending file and directory names to the remote shell.

See `comint-file-name-quote-list' for details."
  :group 'rcp
  :type '(repeat character))

(defcustom rcp-methods
  '( ("rcp"   (rcp-rsh-program "rsh")
              (rcp-rcp-program "rcp")
              (rcp-rsh-args    nil)
              (rcp-rcp-args    nil))
     ("scp"   (rcp-rsh-program "ssh")
              (rcp-rcp-program "scp")
              (rcp-rsh-args    ("-e" "none"))
              (rcp-rcp-args    nil))
     ("rsync" (rcp-rsh-program "ssh")
              (rcp-rcp-program "rsync")
              (rcp-rsh-args    ("-e" "none"))
              (rcp-rcp-args    nil)))
  "*Alist of methods for remote files.
This is a list of entries of the form (name parm1 parm2 ...).
Each name stands for a remote access method.  Each parameter is a
pair of the form (key value).  The following keys are defined:
  rcp-rsh-program       name of program to use for rsh;
                            this might be the full path to rsh or
                            the name of a workalike program
  rcp-rsh-args          list of arguments to pass to above mentioned
                            program
  rcp-rcp-program       name of program to use for rcp;
                            this might be the full path to rcp or
                            the name of a workalike program
  rcp-rcp-args          list of parameters to pass to above mentioned
                            program"
  :group 'rcp
  :type '(repeat
          (cons string
                (set (list (const rcp-rsh-program) string)
                     (list (const rcp-rcp-program) string)
                     (list (const rcp-rsh-args) (repeat string))
                     (list (const rcp-rcp-args) (repeat string))))))

(defcustom rcp-default-method "rsh"
  "*Default method to use for transferring files.
See `rcp-methods' for possibilities."
  :group 'rcp
  :type 'string)

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

(defcustom rcp-rsh-end-of-line "\n"
  "*String used for end of line in rsh connections.
I don't think this ever needs to be changed, so please tell me about it
if you need to change this."
  :group 'rcp
  :type 'string)

(defcustom rcp-remote-path '("/bin" "/usr/bin" "/usr/sbin" "/usr/local/bin")
  "*List of directories to search for executables on remote host.
Please notify me about other semi-standard directories to include here."
  :group 'rcp
  :type '(repeat string))

(defcustom rcp-temp-name-prefix "/tmp/rcp."
  "*Prefix to use for temporary files.
You might wish to use another tmp directory.  Don't forget to include
a prefix for the filename part, though."
  :group 'rcp
  :type 'string)

;; File name format.

(defcustom rcp-file-name-structure
  (list "\\`/r\\(@\\([a-z]+\\)\\)?:\\(\\([a-z0-9_]+\\)@\\)?\\([a-z0-9.-]+\\):\\(.*\\)\\'"
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
    (write-region . rcp-handle-write-region)
    (vc-registered . rcp-handle-vc-registered))
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

;;; File Name Handler Functions:

;; Basic functions.

(defun rcp-handle-file-exists-p (filename)
  "Like `file-exists-p' for rcp files."
  (let ((v (rcp-dissect-file-name filename))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        method user host path)
    (setq method (rcp-file-name-method v))
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (rcp-send-command method user host
                        (format "%s -d %s >/dev/null 2>&1"
                                (rcp-get-ls-command method user host)
                                (comint-quote-filename path)))
      (rcp-send-command method user host "echo $?")
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
         (rcp-file-name-method v)
         (rcp-file-name-user v) (rcp-file-name-host v)
         (format "%s -iLldn %s"
                 (rcp-get-ls-command (rcp-file-name-method v)
                                     (rcp-file-name-user v)
                                     (rcp-file-name-host v))
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
    

;; Directory listings.

(defun rcp-handle-directory-files (directory &optional full match nosort)
  "Like `directory-files' for rcp files."
  (let ((v (rcp-dissect-file-name directory))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        method user host path result x)
    (setq method (rcp-file-name-method v))
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (if full
          (rcp-send-command
           method user host
           (format "%s -ad %s" (rcp-get-ls-command method user host)
                   (comint-quote-filename path)))
        (rcp-send-command method user host
                          (format "cd %s" (comint-quote-filename path)))
        (rcp-send-command
         method user host
         (format "%s -a" (comint-quote-filename
                          (rcp-get-ls-command method user host)))))
      (rcp-wait-for-output)
      (goto-char (point-max))
      (while (zerop (forward-line -1))
        (setq x (buffer-substring (point)
                                  (progn (end-of-line) (point))))
        (if match
            (when (string-match match x) (push x result))
          (push x result))))
    result))

;; This can be made faster by using `ls -l'.  We would then parse the
;; output and use the first character to decide whether it's a
;; directory.  But if we do that, we've got a problem with symlinks.
;; Hm.  OTOH, the rest of rcp.el doesn't grok filenames with spaces in
;; them, either, so...  Hm.
(defun rcp-handle-file-name-all-completions (file directory)
  "Like `file-name-all-completions' for rcp files."
  (let ((v (rcp-dissect-file-name directory))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        method user host path result)
    (setq method (rcp-file-name-method v))
    (setq user (rcp-file-name-user v))
    (setq host (rcp-file-name-host v))
    (setq path (rcp-file-name-path v))
    (save-excursion
      (rcp-send-command method user host (format "cd %s" path))
      (rcp-send-command method user host
                         (format "%s -ad %s* 2>/dev/null"
                                 (rcp-get-ls-command method user host)
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

;; CCC todo
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
  (let* ((v1 (when (rcp-rcp-file-p file)
               (rcp-dissect-file-name file)))
         (v2 (when (rcp-rcp-file-p newname)
               (rcp-dissect-file-name newname)))
         (meth (rcp-file-name-method (or v1 v2)))
         (rcp-args (rcp-get-rcp-args meth)))
    (let ((f1 (if (not v1)
                  file
                (rcp-make-rcp-program-file-name
                 (rcp-file-name-user v1)
                 (rcp-file-name-host v1)
                 (comint-quote-filename (rcp-file-name-path v1)))))
          (f2 (if (not v2)
                  newname
                (rcp-make-rcp-program-file-name
                 (rcp-file-name-user v2)
                 (rcp-file-name-host v2)
                 (comint-quote-filename (rcp-file-name-path v2))))))
      (when keep-date
        (add-to-list 'rcp-args "-p"))
      (apply #'call-process (rcp-get-rcp-program meth) nil nil nil
             (append rcp-args (list f1 f2))))))

;; mkdir
(defun rcp-handle-make-directory (dir &optional parents)
  "Like `make-directory' for rcp files."
  (let ((v (rcp-dissect-file-name dir))
        (comint-file-name-quote-list rcp-file-name-quote-list))
    (rcp-send-command
     (rcp-file-name-method v) (rcp-file-name-user v) (rcp-file-name-host v)
     (format "%s %s"
             (if parents "mkdir -p" "mkdir")
             (comint-quote-filename (rcp-file-name-path v))))))

;; CCC error checking?
(defun rcp-handle-delete-directory (directory)
  "Like `delete-directory' for rcp files."
  (let ((v (rcp-dissect-file-name directory))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        result)
    (save-excursion
      (rcp-send-command
       (rcp-file-name-method v) (rcp-file-name-user v) (rcp-file-name-host v)
       (format "rmdir %s ; echo ok"
               (comint-quote-filename (rcp-file-name-path v))))
      (rcp-wait-for-output))))

(defun rcp-handle-delete-file (filename)
  "Like `delete-file' for rcp files."
  (let ((v (rcp-dissect-file-name filename))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        result)
    (save-excursion
      (rcp-send-command
       (rcp-file-name-method v)
       (rcp-file-name-user v)
       (rcp-file-name-host v)
       (format "rm -f %s ; echo ok"
               (comint-quote-filename (rcp-file-name-path v))))
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
      (rcp-send-command method user host (format "cd %s" path))
      (rcp-send-command method user host
                        (mapconcat #'identity (cons program arguments)))
      (rcp-wait-for-output))
    (unless discard
      (insert-buffer (rcp-get-buffer method user host)))
    (save-excursion
      (rcp-send-command method user host "echo $?")
      (rcp-wait-for-output)
      (read (rcp-get-buffer method user host)))))

(defun rcp-handle-insert-directory
  (file switches &optional wildcard full-directory-p)
  "Like `insert-directory' for rcp files."
  (let ((v (rcp-dissect-file-name file))
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
      (rcp-send-command method user host (format "cd %s" path))
      (rcp-send-command method user host
                        (format "%s %s"
                                (rcp-get-ls-command method user host)
                                switches))
      (sit-for 1)                       ;needed for rsh but not ssh?
      (rcp-wait-for-output))
    (insert-buffer (rcp-get-buffer method user host))))

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
           (method (rcp-file-name-method v))
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
             method user host
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
         method user host
         (rcp-run-real-handler 'expand-file-name (list path)))))))

;; Remote commands.

(defun rcp-handle-shell-command (command &optional output-buffer)
  "Like `shell-command' for rcp files.
Bug: COMMAND must not output the string `/////'.
Bug: output of COMMAND must end with a newline."
  (if (rcp-rcp-file-p default-directory)
      (let* ((v (rcp-dissect-file-name default-directory))
             (comint-file-name-quote-list rcp-file-name-quote-list)
             (method (rcp-file-name-method v))
             (user (rcp-file-name-user v))
             (host (rcp-file-name-host v))
             (path (rcp-file-name-path v)))
        (when (string-match "&[ \t]*\\'" command)
          (error "Rcp doesn't grok asynchronous shell commands, yet."))
        (save-excursion
          (rcp-send-command
           method user host (format "cd %s; pwd" (comint-quote-filename path)))
          (rcp-wait-for-output)
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

(defun rcp-handle-file-local-copy (file)
  "Like `file-local-copy' for rcp files."
  (let ((v (rcp-dissect-file-name filename))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        tmpfil)
    (setq tmpfil (make-temp-name rcp-temp-name-prefix))
    (rcp-message 5 "Fetching %s to tmp file %s..." file tmpfil)
    (apply #'call-process
           (rcp-get-rcp-program (rcp-file-name-method v)) nil nil nil
           (append (rcp-get-rcp-args (rcp-file-name-method v))
                   (list
                    (rcp-make-rcp-program-file-name
                     (rcp-file-name-user v)
                     (rcp-file-name-host v)
                     (comint-quote-filename (rcp-file-name-path v)))
                    tmpfil)))
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
  (let ((v (rcp-dissect-file-name filename))
        (comint-file-name-quote-list rcp-file-name-quote-list)
        tmpfil)
    (setq tmpfil (make-temp-name rcp-temp-name-prefix))
    (rcp-run-real-handler
     'write-region
     (if confirm ; don't pass this arg unless defined for backward compat.
         (list start end tmpfil append 'no-message lockname confirm)
       (list start end tmpfil append 'no-message lockname)))
    (apply #'call-process
           (rcp-get-rcp-program (rcp-file-name-method v)) nil nil nil
           (append (rcp-get-rcp-args (rcp-file-name-method v))
                   (list
                    tmpfil
                    (rcp-make-rcp-program-file-name
                     (rcp-file-name-user v)
                     (rcp-file-name-host v)
                     (comint-quote-filename (rcp-file-name-path v))))))
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
               (comint-file-name-quote-list rcp-file-name-quote-list)
               bufstr)
          (let ((comint-file-name-quote-list
                 (set-difference rcp-file-name-quote-list '(?\* ?\? ?[ ?]))))
            (rcp-send-command method user host
                              (format "echo %s" (comint-quote-filename path)))
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
           (path (rcp-file-name-path v))
           (comint-file-name-quote-list rcp-file-name-quote-list))
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
          (mapconcat 'comint-quote-filename
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

(unless (fboundp 'rcp-original-vc-do-command)
  (fset 'rcp-original-vc-do-command (symbol-function 'vc-do-command)))

(defun vc-do-command (buffer okstatus command file last &rest flags)
  "Redefined to work with rcp.el; see `rcp-original-vc-do-command' for
original definition."
  (if (and (stringp file) (rcp-rcp-file-p file))
      (apply 'rcp-vc-do-command buffer okstatus command file last flags)
    (apply
     'rcp-original-vc-do-command buffer okstatus command file last flags)))

;; `vc-workfile-unchanged-p'
;; This function does not deal well with remote files, so we do the
;; same as for `vc-do-command'.

;; `vc-workfile-unchanged-p' checks the modification time, we cannot
;; do that for remote files, so here's a version which relies on diff.
(defun rcp-vc-workfile-unchanged-p (file &optional want-differences-if-changed)
  (zerop (vc-backend-diff file nil nil
                          (not want-differences-if-changed))))

(unless (fboundp 'rcp-original-vc-workfile-unchanged-p)
  (fset 'rcp-original-vc-workfile-unchanged-p
        (symbol-function 'vc-workfile-unchanged-p)))

(defun vc-workfile-unchanged-p (file &optional want-differences-if-changed)
  (if (and (stringp file) (rcp-rcp-file-p file))
      (apply 'rcp-vc-workfile-unchanged-p
             (list file want-differences-if-changed))
    (apply 'rcp-original-vc-workfile-unchanged-p
           (list file want-differences-if-changed))))


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
        (comint-file-name-quote-list rcp-file-name-quote-list)
        result)
    (save-excursion
      (rcp-send-command
       (rcp-file-name-method v) (rcp-file-name-user v) (rcp-file-name-host v)
       (format "test %s %s ; echo $?" switch
               (comint-quote-filename (rcp-file-name-path v))))
      (rcp-wait-for-output)
      (read (current-buffer)))))

(defun rcp-buffer-name (method user host)
  "A name for the connection buffer for USER at HOST using METHOD."
  (format "*rcp/%s %s@%s*" method user host))

(defun rcp-get-buffer (method user host)
  "Get the connection buffer to be used for USER at HOST using METHOD."
  (get-buffer-create (rcp-buffer-name method user host)))

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

(defun rcp-open-connection-rsh (method user host)
  "Open a connection to HOST, logging in as USER, using METHOD."
  (set-buffer (rcp-get-buffer method user host))
  (erase-buffer)
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
  (rcp-send-command method user host "echo hello")
  (unless (rcp-wait-for-output 5)
    (pop-to-buffer (buffer-name))
    (error "Remote /bin/sh didn't come up.  See buffer `%s' for details."
           (buffer-name)))
  (rcp-message 7 "Waiting for remote /bin/sh to come up...done")
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

(defun rcp-maybe-open-connection-rsh (method user host)
  "Open a connection to HOST, logging in as USER, using METHOD, if none exists."
  (let ((p (get-buffer-process (rcp-get-buffer method user host))))
    (unless (and p
                 (processp p)
                 (memq (process-status p) '(run open)))
      (when (and p (processp p))
        (delete-process p))
      (rcp-open-connection-rsh method user host))))

(defun rcp-send-command (method user host command)
  "Send the COMMAND to USER at HOST (logged in using METHOD)."
  (rcp-maybe-open-connection-rsh method user host)
  (let ((proc nil))
    (set-buffer (rcp-get-buffer method user host))
    (erase-buffer)
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
    (while (progn (goto-char (point-max))
                  (forward-line -1)
                  (not (looking-at (regexp-quote rcp-end-of-output))))
      (setq result (accept-process-output proc timeout)))
    (delete-region (point) (progn (forward-line 1) (point)))
    (goto-char (point-min))
    result))

;; rcp file names

(defstruct rcp-file-name method user host path)

(defun rcp-rcp-file-p (name)
  "Return t iff this is an rcp file."
  (string-match rcp-file-name-regexp name))

(defun rcp-dissect-file-name (name)
  "Returns a vector: remote method, remote user, remote host, remote path name."
  (unless (string-match (nth 0 rcp-file-name-structure) name)
    (error "Not an rcp file name: %s" name))
  (make-rcp-file-name
   :method (or (match-string (nth 1 rcp-file-name-structure) name)
               rcp-default-method)
   :user (or (match-string (nth 2 rcp-file-name-structure) name)
             (user-login-name))
   :host (match-string (nth 3 rcp-file-name-structure) name)
   :path (match-string (nth 4 rcp-file-name-structure) name)))

;; CCC: This must be changed for other rcp file name formats!
(defun rcp-make-rcp-file-name (method user host path &optional str)
  "Constructs an rcp file name from METHOD, USER, HOST and PATH."
  (let ((fmt-alist (list (cons "%%" "%")
                         (cons "%m" method)
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
              (rcp-make-rcp-file-name method user host path c)))))

(defun rcp-make-rcp-program-file-name (user host path)
  "Creates a file name suitable to be passed to `rcp'."
  (format "%s@%s:%s" user host path))

;; Variables local to connection.

(defun rcp-get-ls-command (method user host)
  (save-excursion
    (rcp-maybe-open-connection-rsh method user host)
    (set-buffer (rcp-get-buffer method user host))
    rcp-ls-command))

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
  

;;; TODO:

;; * Make rcp-handle-file-name-all-completions faster.
;; * BSD ls doesn't grok `-n' to print numeric user/group ids.
;; * Make sure permissions of tmp file are good.
;;   (Nelson Minar <nelson@media.mit.edu>)
;; * Automatically find out as much as possible about the remote system.
;;   Maybe it would be best to have a list of directories to search for
;;   executables on all systems, and a list of program names to try?
;;   (E.g. one could try `gnuls' on BSD systems for the numeric user id
;;   thing.)
;;   Francesco suggests that one could have variables which determine
;;   how to do things on remote systems, if there is no match for
;;   the remote system name, rcp.el should guess.
;; * Dired header line contains duplicated directory name.
;; * Use rsync if available.  Fall back to rcp if scp isn't available.
;;   (Francesco PotortÅÏ <F.Potorti@cnuce.cnr.it>)
;; * rcp program name should be customizable on per-host basis?
;;   (Francesco PotortÅÏ <F.Potorti@cnuce.cnr.it>)
;; * Grok passwd prompts.  (David Winter
;;   <winter@nevis1.nevis.columbia.edu>).  Maybe just do `rsh -l user
;;   host', then wait a while for the passwd or passphrase prompt.  If
;;   there is one, remember the passwd/phrase.
;; * Do file transfer via rsh not rcp.  Probably we need to use
;;   uuencode and uudecode for this.  Is this faster than rcp or
;;   rsync?
;; * Find out atime, mtime and ctime of remote file?
;; * Is the dummy `file-truename' function we've got really sufficient?
;; * How to deal with MULE in `insert-file-contents' and `write-region'?
;; * Implement `add-name-to-file'.
;; * Do asynchronous `shell-command's.
;; * Grok `append' and `lockname' parameters for `write-region'.
;; * Test remote ksh or bash for tilde expansion in `rcp-find-shell'?
;; * Put commands and responses in a debug buffer.
;; * vc-user-login-name: whenever it is called from VC, the variable
;;   `file' is bound to the current file name.  Thus, we can change
;;   vc-user-login-name such that it does the right thing with rcp.el
;;   files.
;;   Find out who called me:
;;   (defun foo ()
;;     (let ((caller (backtrace-frame 3)))
;;       (message "%s" (symbol-name (cadr caller)))))
;; * abbreviate-file-name

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
