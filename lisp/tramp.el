;;; rssh.el --- remote file editing using ssh/scp

;; Copyright (C) 1998 by Kai Grossjohann

;; Author: Kai.Grossjohann@CS.Uni-Dortmund.DE
;; Keywords: comm, processes
;; Version: $Id: tramp.el,v 1.27 1999/02/12 17:55:18 grossjoh Exp $

;; rssh.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; rssh.el is distributed in the hope that it will be useful,
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
;; the local and the remote host, whereas rssh.el uses a combination
;; of ssh and scp.
;;
;; Installation is simple -- it is sufficient to load this file.  EFS
;; users should do (require 'efs) before loading this file, though.
;; This is such that the regexes for rssh files come before the
;; regexes for EFS files in `file-name-handler-alist'.
;;
;; Usage is also simple: it's just like ange-ftp, but uses a different
;; syntax for the remote file names.  The syntax used is as follows:
;;
;; /s:USER@HOST:FILENAME
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
;; Firstly, you MUST have set up ssh such that you can log in as USER
;; to the remote HOST without entering a password or pass phrase.  The
;; code does *not* check if you have done this, so expect strange
;; effects when ssh asks you for a password or pass phrase.  (It is
;; fairly easy to allow login without entering a password or pass
;; phrase by using ssh-agent, or by using the .shosts file, or even
;; the .rhosts file.  Thus, I did not deem it important to deal with
;; querying for passwords in this early stage of development.)
;;
;; Secondly, there is almost no error checking at all.
;;
;; I have thus far tested this on one machine, as one user, with two
;; files and one directory.
;;
;; Here's what seems to work so far:
;;   - typing C-x C-f then entering a complete absolute file name
;;   - filename completion (for absolute file names)
;;   - saving files
;;   - getting a directory listing with dired (But why does `f' not work?)


;;; TODO:

;; * Use more variables for program names.
;; * Make it possible to make program names dependent on system type
;;   as well as host name.
;; * Is it cleaner to use whole commands rather than just abbrevs for
;;   the binary?
;; * BSD doesn't grok `-n' to print numeric user/group ids.

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

;;; Code:

(require 'cl)
;; Emacs 19.34 compatibility hack -- is this needed?
(or (>= emacs-major-version 20)
    (load "cl-seq"))

(provide 'rssh)

;;; User Customizable Internal Variables:

(defvar rssh-ssh-program "ssh"
  "*Name of ssh program.")

(defvar rssh-ssh-args '("-e" "none")
  "*Args for running ssh.")

(defvar rssh-scp-program "scp"
  "*Name of scp program.")

(defvar rssh-ssh-end-of-line "\n"
  "*String used for end of line in ssh connections.")

(defvar rssh-sh-command-alist
  '(("" . "/bin/ksh"))
  "*Alist saying what command is used to invoke `sh' for each host.
The key is a regex matched against the host name, the value is the
name to use for `sh', which should be a Bourne shell.

This shell should grok tilde expansion, i.e. `cd ~username' should do
something useful.  Modern Bourne shells seem to do this, but maybe
you need to use `ksh' as a shell, or `bash'.")

(defvar rssh-ls-command-alist
  '(("" . "ls"))
  "*Alist saying what command is used to invoke `ls' for each host.
The key is a regex matched against the host name, the value is the
name to use for `ls'.")

(defvar rssh-cd-command-alist
  '(("" . "cd"))
  "*Alist saying what command is used to invoke `cd' for each host.
The key is a regex matched against the host name, the value is the
name to use for `cd'.")

(defvar rssh-test-command-alist
  '(("" . "test"))
  "*Alist saying what command is used to invoke `test' for each host.
The key is a regex matched against the host name, the value is the
name to use for `test'.")

(defvar rssh-mkdir-command-alist
  '(("" . "mkdir"))
  "*Alist saying what command is used to invoke `mkdir' for each host.
The key is a regex matched against the host name, the value is the
name to use for `mkdir'.
This does not need to create parent directories.")

(defvar rssh-mkdir-p-command-alist
  '(("" . "mkdir -p"))
  "*Alist saying what command is used to invoke `mkdir -p' for each host.
The key is a regex matched against the host name, the value is the
name to use for `mkdir -p'.
`mkdir -p' should create parent directories which do not exist.")

(defvar rssh-rmdir-command-alist
  '(("" . "rmdir"))
  "*Alist saying what command is used to invoke `rmdir' for each host.
The key is a regex matched against the host name, the value is the
name to use for `rmdir'.")

(defvar rssh-rm-f-command-alist
  '(("" . "rm -f"))
  "*Alist saying what command is used to invoke `rm -f' for each host.
The key is a regex matched against the host name, the value is the
name to use for `rm -f'.
This command should produce as little output as possible, hence `-f'.")

;; File name format.

(defvar rssh-rssh-file-name-structure
  (list "\\`/s:\\(\\([a-z0-9_]+\\)@\\)?\\([a-z0-9.-]+\\):\\(.*\\)\\'"
        2 3 4)
  "*List of four elements, detailing the rssh file name structure.

The first element is a regular expression matching an rssh file name.
The regex should contain parentheses around the user name, the host
name, and the file name parts.

The second element is a number, saying which pair of parentheses matches
the user name.  The third element is similar, but for the host name.
The fourth element is for the file name.  These numbers are passed
directly to `match-string', which see.  That means the opening parentheses
are counted to identify the pair.

See also `rssh-rssh-file-name-regexp' and `rssh-make-rssh-file-format'.")

(defvar rssh-rssh-file-name-regexp "\\`/s:"
  "*Regular expression matching rssh file names.
This regexp should match rssh file names but no other file names.
\(When rssh.el is loaded, this regular expression is prepended to
`file-name-handler-alist', and that is searched sequentially.  Thus,
if the rssh entry appears rather early in the `file-name-handler-alist'
and is a bit too general, then some files might be considered rssh
files which are not really rssh files.

Please note that the entry in `file-name-handler-alist' is made when
this file (rssh.el) is loaded.  This means that this variable must be set
before loading rssh.el.  Alternatively, `file-name-handler-alist' can be
updated after changing this variable.

Also see `rssh-rssh-file-name-structure' and `rssh-make-rssh-file-format'.")

(defvar rssh-make-rssh-file-format "/s:%u@%h:%p"
  "*Format string saying how to construct rssh file name.
%u is replaced by user name.
%h is replaced by host name.
%p is replaced by file name.
%% is replaced by %.

Also see `rssh-rssh-file-name-structure' and `rssh-rssh-file-name-regexp'.")

;;; Internal Variables:

(defvar rssh-end-of-output "/////"
  "String used to recognize end of output.")

;; New handlers should be added here.
(defconst rssh-file-name-handler-alist
  '((file-exists-p . rssh-handle-file-exists-p)
    (file-directory-p . rssh-handle-file-directory-p)
    (file-executable-p . rssh-handle-file-executable-p)
    (file-accessible-directory-p . rssh-handle-file-accessible-directory-p)
    (file-readable-p . rssh-handle-file-readable-p)
    (file-regular-p . rssh-handle-file-regular-p)
    (file-symlink-p . rssh-handle-file-symlink-p)
    (file-writable-p . rssh-handle-file-writable-p)
    (file-attributes . rssh-handle-file-attributes)
    (file-directory-files . rssh-handle-file-directory-files)
    (file-name-all-completions . rssh-handle-file-name-all-completions)
    (add-name-to-file . rssh-handle-add-name-to-file)
    (copy-file . rssh-handle-copy-file)
    (make-directory . rssh-handle-make-directory)
    (delete-directory . rssh-handle-delete-directory)
    (delete-file . rssh-handle-delete-file)
    (dired-call-process . rssh-handle-dired-call-process)
    ;;(shell-command . rssh-handle-shell-command)
    (insert-directory . rssh-handle-insert-directory)
    (expand-file-name . rssh-handle-expand-file-name)
    (file-local-copy . rssh-handle-file-local-copy)
    (insert-file-contents . rssh-handle-insert-file-contents)
    (write-region . rssh-handle-write-region))
        "List of handler functions.")

;;; File Name Handler Functions:

;; Basic functions.

(defun rssh-handle-file-exists-p (filename)
  "Like `file-exists-p' for rssh files."
  (let ((v (rssh-dissect-file-name filename))
        user host path)
    (setq user (rssh-file-name-user v))
    (setq host (rssh-file-name-host v))
    (setq path (rssh-file-name-path v))
    (save-excursion
      (rssh-send-command user host
                         (format "%s -d '%s' >/dev/null 2>&1"
                                 (rssh-ls-command-get host) path))
      (rssh-send-command user host "echo $?")
      (rssh-wait-for-output)
      (zerop (read (current-buffer))))))

(defun rssh-handle-file-attributes (filename)
  "Like `file-attributes' for rssh files."
  (let ((v (rssh-dissect-file-name filename))
        user host path symlinkp dirp
        res-inode res-filemodes res-numlinks
        res-uid res-gid res-size res-symlink-target)
    (if (not (rssh-handle-file-exists-p filename))
        nil                             ; file cannot be opened
      ;; file exists, find out stuff
      (save-excursion
        (rssh-send-command
         (rssh-file-name-user v) (rssh-file-name-host v)
         (format "%s -iLldn %s"
                 (rssh-ls-command-get (rssh-file-name-host v))
                 (rssh-file-name-path v)))
        (rssh-wait-for-output)
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

(defun rssh-handle-file-executable-p (filename)
  "Like `file-executable-p' for rssh files."
  (zerop (rssh-run-test "-x" filename)))

(defun rssh-handle-file-readable-p (filename)
  "Like `file-readable-p' for rssh files."
  (zerop (rssh-run-test "-r" filename)))

(defun rssh-handle-file-accessible-directory-p (filename)
  "Like `file-accessible-directory-p' for rssh files."
  (and (zerop (rssh-run-test "-d" filename))
       (zerop (rssh-run-test "-r" filename))
       (zerop (rssh-run-test "-x" filename))))

;; Functions implemented using basic functions.

(defun rssh-handle-file-directory-p (filename)
  (eq t (car (rssh-handle-file-attributes filename))))

(defun rssh-handle-file-regular-p (filename)
  "Like `file-regular-p' for rssh files."
  (eq ?- (aref (nth 8 (rssh-handle-file-attributes filename)) 0)))

(defun rssh-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for rssh files."
  (stringp (car (rssh-handle-file-attributes filename))))

(defun rssh-handle-file-writable-p (filename)
  "Like `file-writable-p' for rssh files."
  (if (rssh-handle-file-exists-p filename)
      ;; Existing files must be writable.
      (zerop (rssh-run-test "-w" filename))
    ;; If file doesn't exist, check if directory is writable.
    (and (zerop (rssh-run-test "-d" (file-name-nondirectory filename)))
         (zerop (rssh-run-test "-w" (file-name-nondirectory filename))))))
       

;; Other file name ops.

(defun rssh-handle-file-truename (filename &optional counter prev-dirs)
  "Like `file-truename' for rssh files."
  filename)                             ;CCC what to do?
    

;; Directory listings.

(defun rssh-handle-directory-files (directory &optional full match nosort)
  "Like `directory-files' for rssh files."
  (let ((v (rssh-dissect-file-name directory))
        user host path result x)
    (setq user (rssh-file-name-user v))
    (setq host (rssh-file-name-host v))
    (setq path (rssh-file-name-path v))
    (save-excursion
      (if full
          (rssh-send-command
           user host (format "%s -ad %s" (rssh-ls-command-get host) path))
        (rssh-send-command user host (rssh-cd-command host path))
        (rssh-send-command user host (format "%s -a"
                                             (rssh-ls-command-get host))))
      (rssh-wait-for-output)
      (goto-char (point-max))
      (while (zerop (forward-line -1))
        (setq x (buffer-substring (point)
                                  (progn (end-of-line) (point))))
        (if match
            (when (string-match match x) (push x result))
          (push x result))))
    result))

(defun rssh-handle-file-name-all-completions (file directory)
  "Like `file-name-all-completions' for rssh files."
  (let ((v (rssh-dissect-file-name directory))
        user host path result)
    (setq user (rssh-file-name-user v))
    (setq host (rssh-file-name-host v))
    (setq path (rssh-file-name-path v))
    (save-excursion
      (rssh-send-command user host (rssh-cd-command host path))
      (rssh-send-command user host
                         (format "%s -adF %s* 2>/dev/null"
                                 (rssh-ls-command-get host) file))
      (rssh-wait-for-output)
      (goto-char (point-max))
      (while (zerop (forward-line -1))
        (push (buffer-substring (point)
                                (progn (end-of-line) (point)))
              result)))
    result))

;; cp, mv and ln
(defun rssh-handle-add-name-to-file
  (file newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for rssh files."
  (error "add-name-to-file not implemented yet for rssh files."))

(defun rssh-handle-copy-file
  (file newname &optional ok-if-already-exists)
  "Like `copy-file' for rssh files."
  (error "copy-file not implemented yet for rssh files."))

;; mkdir
(defun rssh-handle-make-directory (dir &optional parents)
  "Like `make-directory' for rssh files."
  (let ((v (rssh-dissect-file-name dir))
        host)
    (setq host (rssh-file-name-host v))
    (rssh-send-command
     (rssh-file-name-user v) host
     (format "%s %s"
             (if parents
                 (rssh-mkdir-p-command-get host)
               (rssh-mkdir-command-get host))
             (rssh-file-name-path v)))))

;; error checking?
(defun rssh-handle-delete-directory (directory)
  "Like `delete-directory' for rssh files."
  (let ((v (rssh-dissect-file-name directory))
        host result)
    (setq host (rssh-file-name-host v))
    (save-excursion
      (rssh-send-command (rssh-file-name-user v) host
                         (format "%s %s ; echo ok"
                                 (rssh-rmdir-command-get host)
                                 (rssh-file-name-path v)))
      (rssh-wait-for-output))))

(defun rssh-handle-delete-file (filename)
  "Like `delete-file' for rssh files."
  (let ((v (rssh-dissect-file-name filename))
        host result)
    (setq host (rssh-file-name-host v))
    (save-excursion
      (rssh-send-command (rssh-file-name-user v)
                         (rssh-file-name-host v)
                         (format "%s %s ; echo ok"
                                 (rssh-rm-f-command-get host)
                                 (rssh-file-name-path v)))
      (rssh-wait-for-output))))

;; Dired.

(defun rssh-handle-dired-call-process (program discard &rest arguments)
  "Like `dired-call-process' for rssh files."
  (let ((v (rssh-dissect-file-name default-directory))
        user host path result)
    (setq user (rssh-file-name-user v))
    (setq host (rssh-file-name-host v))
    (setq path (rssh-file-name-path v))
    (save-excursion
      (rssh-send-command user host path)
      (rssh-send-command user host
                         (mapconcat #'identity (cons program arguments)))
      (rssh-wait-for-output))
    (unless discard
      (insert-buffer (rssh-get-buffer user host)))
    (save-excursion
      (rssh-send-command user host "echo $?")
      (rssh-wait-for-output)
      (read (rssh-get-buffer user host)))))

(defun rssh-handle-insert-directory
  (file switches &optional wildcard full-directory-p)
  "Like `insert-directory' for rssh files."
  (let ((v (rssh-dissect-file-name file))
        user host path)
    (setq user (rssh-file-name-user v))
    (setq host (rssh-file-name-host v))
    (setq path (rssh-file-name-path v))
    (when (listp switches)
      (setq switches (mapconcat #'identity switches " ")))
    (unless full-directory-p
      (setq switches (concat "-d " switches)))
    (save-excursion
      (rssh-send-command user host
                         (rssh-cd-command host path))
      (rssh-send-command user host
                         (rssh-ls-command host switches ""))
      (rssh-wait-for-output))
    (insert-buffer (rssh-get-buffer user host))))

;; Canonicalization of file names.

(defun rssh-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for rssh files."
  ;; If DIR is not given, use DEFAULT-DIRECTORY or "/".
  (setq dir (or dir default-directory "/"))
  ;; Unless NAME is absolute, concat DIR and NAME.
  (unless (file-name-absolute-p name)
    (setq name (concat (file-name-as-directory dir) name)))
  ;; If NAME is not an rssh file, run the real handler
  (if (not (rssh-rssh-file-p name))
      (rssh-run-real-handler 'expand-file-name
                             (list name nil))
    ;; Dissect NAME.
    (let* ((v (rssh-dissect-file-name name))
           (user (rssh-file-name-user v))
           (host (rssh-file-name-host v))
           (path (rssh-file-name-path v)))
      (unless (file-name-absolute-p path)
        (setq path (concat "~/" path)))
      (save-excursion
        ;; Tilde expansion if necessary.  This needs a shell
        ;; which groks tilde expansion!  Maybe you need to set
        ;; rssh-sh-command-alist to /usr/bin/ksh for some hosts
        ;; where sh is too stupid?
        (when (string-match "\\`\\(~[^/]*\\)\\(.*\\)\\'" path)
          (let ((uname (match-string 1 path))
                (fname (match-string 2 path)))
            (rssh-send-command
             user host
             (format "cd %s; pwd" uname))
            (rssh-wait-for-output)
            (goto-char (point-min))
            (setq uname (buffer-substring (point)
                                          (progn (end-of-line)
                                                 (point))))
            (setq path (concat uname fname))))
        ;; No tilde characters in file name, do normal
        ;; expand-file-name (this does "/./" and "/../").
        (rssh-make-rssh-file-name
         user host
         (rssh-run-real-handler 'expand-file-name (list path)))))))

;; Remote commands.

;;-(defun rssh-handle-shell-command (command &optional output-buffer)
;;-  "Like `shell-command' for rssh files."
;;-  (message "Not implemented yet."))

;; File Editing.

(defun rssh-handle-file-local-copy (file)
  "Like `file-local-copy' for rssh files."
  (let ((v (rssh-dissect-file-name filename))
        tmpfil)
    (setq tmpfil (make-temp-name "/tmp/rssh."))
    (call-process rssh-scp-program nil nil nil
                  (format "%s@%s:%s"
                          (rssh-file-name-user v)
                          (rssh-file-name-host v)
                          (rssh-file-name-path v))
                  tmpfil)
    tmpfil))

;; CCC need to do MULE stuff
(defun rssh-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for rssh files."
  (let ((local-copy (rssh-handle-file-local-copy filename)))
    (when visit
      (setq buffer-file-name filename)
      (set-visited-file-modtime '(0 0))
      (set-buffer-modified-p nil))
    (rssh-run-real-handler 'insert-file-contents
                           (list local-copy nil beg end replace))
    (delete-file local-copy)
    ))

(defun rssh-handle-write-region
  (start end filename &optional append visit lockname confirm)
  "Like `write-region' for rssh files."
  (unless (eq visit t)
    (error "rssh-handle-write-region: VISIT must be t."))
  (unless (eq append nil)
    (error "rssh-handle-write-region: APPEND must be nil."))
  (unless (eq lockname nil)
    (error "rssh-handle-write-region: LOCKNAME must be nil."))
  (unless (eq confirm nil)
    (error "rssh-handle-write-region; CONFIRM must be nil."))
  (let ((v (rssh-dissect-file-name filename))
        tmpfil)
    (setq tmpfil (make-temp-name "/tmp/rssh."))
    (rssh-run-real-handler
     'write-region
     (list start end tmpfil append 'no-message lockname confirm))
    (call-process rssh-scp-program nil nil nil
                  tmpfil
                  (format "%s@%s:%s"
                          (rssh-file-name-user v)
                          (rssh-file-name-host v)
                          (rssh-file-name-path v)))
    (delete-file tmpfil)
    (message "Wrote %s" filename)))

;; Main function.
(defun rssh-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers
	 (cons 'rssh-file-name-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

(defun rssh-file-name-handler (operation &rest args)
  (let ((fn (assoc operation rssh-file-name-handler-alist)))
    (if fn
        (apply (cdr fn) args)
      (rssh-run-real-handler operation args))))

;; Register in file name handler alist

(add-to-list 'file-name-handler-alist
             (cons rssh-rssh-file-name-regexp 'rssh-file-name-handler))

;;; Internal Functions:

(defun rssh-run-test (switch filename)
  "Run `test' on the remote system, given a switch and a file.
Returns the exit code of test."
  (let ((v (rssh-dissect-file-name filename))
        host result)
    (setq host (rssh-file-name-host v))
    (save-excursion
      (rssh-send-command (rssh-file-name-user v)
                         host
                         (format "%s %s \"%s\" ; echo $?"
                                 (rssh-test-command-get host)
                                 switch
                                 (rssh-file-name-path v)))
      (rssh-wait-for-output)
      (read (current-buffer)))))

(defun rssh-buffer-name (user host)
  "A name for the connection buffer for USER at HOST."
  (format "*rssh %s@%s*" user host))

(defun rssh-get-buffer (user host)
  "Get the connection buffer to be used for USER at HOST."
  (get-buffer-create (rssh-buffer-name user host)))

;; -- communication with external shell -- 

(defun rssh-open-connection-ssh (user host)
  "Open a connection to HOST, logging in as USER, using ssh."
  (set-buffer (rssh-get-buffer user host))
  (erase-buffer)
  (apply 'start-process
         (rssh-buffer-name user host)
         (rssh-get-buffer user host) 
         rssh-ssh-program
         (append rssh-ssh-args
                 (list "-l" user host (rssh-sh-command-get host))))
  ;; Gross hack for synchronization.  How do we do this right?
  (rssh-send-command user host "echo hello")
  (rssh-wait-for-output))

(defun rssh-maybe-open-connection-ssh (user host)
  "Open a connection to HOST, logging in as USER, using ssh, if none exists."
  (let ((p (get-buffer-process (rssh-get-buffer user host))))
    (unless (and p
                 (processp p)
                 (memq (process-status p) '(run open)))
      (rssh-open-connection-ssh user host))))

(defun rssh-send-command (user host command)
  "Send the COMMAND to USER at HOST."
  (rssh-maybe-open-connection-ssh user host)
  (let ((proc nil))
    (set-buffer (rssh-get-buffer user host))
    (erase-buffer)
    (setq proc (get-buffer-process (current-buffer)))
    (process-send-string proc
                         (concat command rssh-ssh-end-of-line))))

(defun rssh-wait-for-output ()
  "Wait for output from remote ssh command."
  (let ((proc (get-buffer-process (current-buffer))))
    (process-send-string proc
                         (format "echo %s%s"
                                 rssh-end-of-output
                                 rssh-ssh-end-of-line))
    (while (progn (goto-char (point-max))
                  (forward-line -1)
                  (not (looking-at (regexp-quote rssh-end-of-output))))
      (accept-process-output proc))
    (kill-line 1)
    (goto-char (point-min))))

;; rssh file names

(defstruct rssh-file-name user host path)

(defun rssh-rssh-file-p (name)
  "Return t iff this is an rssh file."
  (string-match rssh-rssh-file-name-regexp name))

(defun rssh-dissect-file-name (name)
  "Returns a vector: remote user, remote host, remote path name."
  (unless (string-match (nth 0 rssh-rssh-file-name-structure) name)
    (error "Not an rssh file name: %s" name))
  (make-rssh-file-name
   :user (or (match-string (nth 1 rssh-rssh-file-name-structure)
                           name)
             (user-login-name))
   :host (match-string (nth 2 rssh-rssh-file-name-structure)
                       name)
   :path (match-string (nth 3 rssh-rssh-file-name-structure)
                       name)))

;; CCC: This must be changed for other rssh file name formats!
(defun rssh-make-rssh-file-name (user host path &optional str)
  "Constructs an rssh file name from USER, HOST and PATH."
  (let ((fmt-alist (list (cons "%%" "%")
                         (cons "%u" user)
                         (cons "%h" host)
                         (cons "%p" path)))
        (m (string-match "\\([^%]*\\)\\(%.\\)\\(.*\\)"
                         (or str rssh-make-rssh-file-format)))
        a b c x)
    (unless rssh-make-rssh-file-format
      (error "`rssh-make-rssh-file-format' is nil"))
    (if (not m)
        ;; return accumulated string if not match
        (or str
            (error "rssh-make-rssh-file-format doesn't contain % escapes"))
      (setq x (or str rssh-make-rssh-file-format))
      (setq a (match-string 1 x))
      (setq b (match-string 2 x))
      (setq c (match-string 3 x))
      (concat a
              (cdr (or (assoc b fmt-alist)
                       (error "Unknown format code: %s" b)))
              (rssh-make-rssh-file-name user host path c)))))

;; Extract right value of alists, depending on host name.

(defsubst rssh-alist-get (string alist)
  "Return the value from the alist, based on regex matching against the keys."
  (cdr (assoc* string alist
               :test (function (lambda (a b)
                                 (string-match b a))))))

(defsubst rssh-sh-command-get (host)
  "Return the `sh' command name for HOST.  See `rssh-sh-command-alist'."
  (rssh-alist-get host rssh-sh-command-alist))

(defsubst rssh-ls-command-get (host)
  "Return the `ls' command name for HOST.  See `rssh-ls-command-alist'."
  (rssh-alist-get host rssh-ls-command-alist))

(defsubst rssh-ls-command (host switches file)
  "Return `ls' command for HOST with SWITCHES on FILE.
SWITCHES is a string."
  (format "%s %s %s"
          (rssh-ls-command-get host)
          switches
          file))

(defsubst rssh-cd-command-get (host)
  "Return the `cd' command name for HOST.  See `rssh-cd-command-alist'."
  (rssh-alist-get host rssh-cd-command-alist))

(defsubst rssh-cd-command (host dir)
  "Return the `cd' command for HOST with DIR."
  (format "%s '%s'" (rssh-cd-command-get host) dir))

(defsubst rssh-test-command-get (host)
  "Return the `test' command name for HOST.  See `rssh-test-command-alist'."
  (rssh-alist-get host rssh-test-command-alist))

(defsubst rssh-mkdir-command-get (host)
  "Return the `mkdir' command name for HOST.  See `rssh-mkdir-command-alist'."
  (rssh-alist-get host rssh-mkdir-command-alist))

(defsubst rssh-mkdir-p-command-get (host)
  "Return the `mkdir -p' command name for HOST.  See `rssh-mkdir-p-command-alist'."
  (rssh-alist-get host rssh-mkdir-p-command-alist))

(defsubst rssh-rmdir-command-get (host)
  "Return the `rmdir' command name for HOST.  See `rssh-rmdir-command-alist'."
  (rssh-alist-get host rssh-rmdir-command-alist))

(defsubst rssh-rmdir-command (host dir)
  "Return the `rmdir' command for HOST with DIR."
  (format "%s '%s'" (rssh-rmdir-command-get host) dir))

(defsubst rssh-rm-f-command-get (host)
  "Return the `rm -f' command name for HOST.  See `rssh-rm-f-command-alist'."
  (rssh-alist-get host rssh-rm-f-command-alist))

;;; rssh.el ends here
