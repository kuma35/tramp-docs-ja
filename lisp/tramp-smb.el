;;; tramp-smb.el --- Tramp access functions for SMB servers -*- coding: iso-8859-1; -*-

;; Copyright (C) 2002 Free Software Foundation, Inc.

;; Author: Michael Albinus <Michael.Albinus@alcatel.de>
;; Keywords: comm, processes

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

;; Access functions for SMB servers like SAMBA or M$ based operating systems
;; from Tramp.

;;; Code:

(require 'tramp)

(eval-when-compile
  (require 'cl)
  (require 'custom)
  ;; Emacs 19.34 compatibility hack -- is this needed?
  (or (>= emacs-major-version 20)
      (load "cl-seq")))

(defvar tramp-use-smb nil
  "*Temporary variable to pretend SMB application as long as under development.
Change it only if you know what you do.")

;; Define SMB method ...
(defcustom tramp-smb-method "smb"
  "*Method to connect SAMBA and M$ SMB servers."
  :group 'tramp
  :type 'string)

;; ... and add it to the method list.
(when tramp-use-smb
  (add-to-list 'tramp-methods (cons tramp-smb-method nil)))

;; Add completion function for SMB method.
(when tramp-use-smb
  (tramp-set-completion-function
   tramp-smb-method
   '((tramp-parse-netrc "~/.netrc"))))

(defcustom tramp-smb-program "smbclient"
  "*Name of SMB client to run."
  :group 'tramp
  :type 'string)

(defconst tramp-smb-prompt "^smb: \\S-+> "
  "Regexp used as prompt in smbclient.")

(defconst tramp-smb-line
  (mapconcat
   'identity
   '("^"
     "\\(\\S-+\\)"                ; file name
     "\\([ADHRS]*\\)"             ; permissions
     "\\([0-9]+\\)"               ; size
     "\\(\\w+\\)"                 ; weekday
     "\\(\\w+\\)"                 ; month
     "\\([0-9]+\\)"               ; day
     "\\([0-9]+:[0-9]+\\):[0-9]+" ; time
     "\\([0-9]+\\)$")             ; year
   "\\s-+")
  "Regexp describing line format of DIR listings in smbclient.")

(defconst tramp-smb-errors
  (mapconcat
   'identity
   '("ERRSRV" "ERRbadpw" "ERRDOS") ; Samba
   "\\|")
  "Regexp for possible error strings of SMB servers.
Used instead of analyzing error codes of commands.")

;; New handlers should be added here.
(defconst tramp-smb-file-name-handler-alist
  '(
    ;; `access-file' performed by default handler
    (add-name-to-file . tramp-smb-not-handled)
    ;; `byte-compiler-base-file-name' performed by default handler
    (copy-file . tramp-smb-not-handled)
    (delete-directory . tramp-smb-not-handled)
    (delete-file . tramp-smb-not-handled)
    (diff-latest-backup-file . tramp-smb-not-handled)
    ;; `directory-file-name' performed by default handler
    (directory-files . tramp-smb-not-handled)
    (directory-files-and-attributes . tramp-smb-not-handled)
    (dired-call-process . tramp-smb-not-handled)
    (dired-compress-file . tramp-smb-not-handled)
    ;; `dired-uncache' performed by default handler
    ;; `expand-file-name' not necessary because we cannot expand "~/"
    (file-accessible-directory-p . tramp-smb-handle-file-directory-p)
    (file-attributes . tramp-smb-not-handled)
    (file-directory-p .  tramp-smb-handle-file-directory-p)
    (file-executable-p . tramp-smb-handle-file-exists-p)
    (file-exists-p . tramp-smb-handle-file-exists-p)
    (file-local-copy . tramp-smb-handle-file-local-copy)
    (file-modes . tramp-smb-not-handled)
    (file-name-all-completions . tramp-smb-not-handled)
    ;; `file-name-as-directory' performed by default handler
    (file-name-completion . tramp-smb-not-handled)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler
    (file-newer-than-file-p . tramp-smb-not-handled)
    (file-ownership-preserved-p . tramp-smb-not-handled)
    (file-readable-p . tramp-smb-handle-file-exists-p)
    (file-regular-p . tramp-smb-not-handled)
    (file-symlink-p . tramp-smb-handle-file-symlink-p)
    ;; `file-truename' performed by default handler
    (file-writable-p . tramp-smb-handle-file-writable-p)
    (find-backup-file-name . tramp-smb-not-handled)
    (find-file-noselect . tramp-smb-not-handled)
    ;; `get-file-buffer' performed by default handler
    (insert-directory . tramp-smb-handle-insert-directory)
    (insert-file-contents . tramp-handle-insert-file-contents)
    (load . tramp-smb-not-handled)
    (make-directory . tramp-smb-not-handled)
    (make-directory-internal . tramp-smb-not-handled)
    (make-symbolic-link . tramp-smb-not-handled)
    (rename-file . tramp-smb-not-handled)
    (set-file-modes . tramp-smb-not-handled)
    (set-visited-file-modtime . tramp-smb-not-handled)
    (shell-command . tramp-smb-not-handled)
    ;; `substitute-in-file-name' performed by default handler
    (unhandled-file-name-directory . tramp-handle-unhandled-file-name-directory)
    ;; `vc-registered' performed by default handler
    (verify-visited-file-modtime . tramp-smb-not-handled)
    (write-region . tramp-smb-not-handled)
)
  "Alist of handler functions for Tramp SMB method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defun tramp-smb-file-name-p (filename)
  "Check if it's a filename for SMB servers."
  (let ((v (tramp-dissect-file-name filename)))
    (string=
     (tramp-find-method
      (tramp-file-name-multi-method v)
      (tramp-file-name-method v)
      (tramp-file-name-user v)
      (tramp-file-name-host v))
     tramp-smb-method)))

(defun tramp-smb-file-name-handler (operation &rest args)
  "Invoke the SMB related OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (let ((fn (assoc operation tramp-smb-file-name-handler-alist)))
    (if fn
	(if (equal (cdr fn) 'tramp-smb-not-handled)
	    (apply (cdr fn) operation args)
	  (save-match-data (apply (cdr fn) args)))
      (tramp-run-real-handler operation args))))

(when tramp-use-smb
  (add-to-list 'tramp-foreign-file-name-handler-alist
	       (cons 'tramp-smb-file-name-p 'tramp-smb-file-name-handler)))

;; File name primitives

(defun tramp-smb-not-handled (operation &rest args)
  "Default handler for all functions.
Used during development in order to get an impression what's left to do."
  (tramp-message 1 "Not handled yet: %s %s" operation args)
  (tramp-run-real-handler operation args))

(defun tramp-handle-file-attributes (filename &optional nonnumeric)
  "Like `file-attributes' for tramp files.
Optional argument NONNUMERIC means return user and group name
rather than as numbers."
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (let* ((share (tramp-smb-get-share path))
	     (file  (tramp-smb-get-path  path))
	     (entry (tramp-smb-get-file-entry user host share file)))
	; check result
	(and (nth 1 entry)
	     (string-match "D" (nth 1 entry)))))))

(defun tramp-smb-handle-file-directory-p (filename)
  "Like `file-directory-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (let* ((share (tramp-smb-get-share path))
	     (file  (tramp-smb-get-path  path))
	     (entry (tramp-smb-get-file-entry user host share file)))
	; check result
	(and (nth 1 entry)
	     (string-match "D" (nth 1 entry)))))))

(defun tramp-smb-handle-file-exists-p (filename)
  "Like `file-exists-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (let* ((share (tramp-smb-get-share path))
	     (file  (tramp-smb-get-path  path))
	     (entry (tramp-smb-get-file-entry user host share file)))
	; check result
	(and file (nth 0 entry)
	     (string-equal (file-name-nondirectory file) (nth 0 entry)))))))

(defun tramp-smb-handle-file-local-copy (filename)
  "Like `file-local-copy' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (let ((share (tramp-smb-get-share path))
	  (file  (tramp-smb-get-path  path))
	  (tmpfil (tramp-make-temp-file)))
      (unless (file-exists-p filename)
	(error "Cannot make local copy of non-existing file `%s'" filename))
      (tramp-message-for-buffer
       nil tramp-smb-method user host
       5 "Fetching %s to tmp file %s..." filename tmpfil)
      (tramp-smb-maybe-open-connection user host share)
      (tramp-smb-send-command user host (format "get %s %s" file tmpfil))
      (tramp-message-for-buffer
       nil tramp-smb-method user host
       5 "Fetching %s to tmp file %s...done" filename tmpfil)
    tmpfil)))

(defun tramp-smb-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for tramp files."
  nil)

(defun tramp-smb-handle-file-writable-p (filename)
  "Like `file-writable-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (let* ((share (tramp-smb-get-share path))
	     (file  (tramp-smb-get-path  path))
	     (entry (tramp-smb-get-file-entry user host share file)))
	; check result
	(and (nth 1 entry)
	     (not (string-match "R" (nth 1 entry))))))))

(defun tramp-smb-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for tramp files.
SWITCHES, WILDCARD and FULL-DIRECTORY-P are not handled."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (save-match-data
	(let* ((share (tramp-smb-get-share path))
	       (file  (tramp-smb-get-path  path))
	       entry list)
	  (tramp-message-for-buffer
	   nil tramp-smb-method user host 10
	   "Inserting directory `dir %s'" path)
	  (tramp-smb-maybe-open-connection user host share)
	  (tramp-smb-send-command user host (format "dir %s*" file))
	  (insert-buffer-substring
	   (tramp-get-buffer nil tramp-smb-method user host))

	  ;; read entries
	  (beginning-of-buffer)
	  (while (setq entry (tramp-smb-read-file-entry))
	    (add-to-list 'list entry)
	    (delete-region (point) (1+ (tramp-point-at-eol))))

	  (delete-region (point) (re-search-forward "\\s-+"))
	  (forward-line 1)
	  (delete-region (point) (point-max))

	  ; print entries
	  (mapcar
	   '(lambda (x)
	      (let ((mode (concat
		      (if (string-match "D" (nth 1 x)) "d" "-") "r"
		      (if (string-match "R" (nth 1 x)) "-" "w") "x------")))
		(insert
		 (format
		  "%10s %3d %-8s %-8s %8s %3s %2s %5s %s\n"
		  mode 1 "nouser" "nogroup"
		  (nth 2 x) ; size
		  (nth 3 x) ; month
		  (nth 4 x) ; day
		  (if (equal (format-time-string "%Y")
			     (nth 6 x))
		      (nth 5 x) ; time
		    (nth 6 x)) ; year
		  (nth 0 x))) ; file name
		(forward-line)
		(beginning-of-line)))
	   (sort list '(lambda (x y) (string-lessp (nth 0 x) (nth 0 y))))))))))

;; Internal file name functions

(defun tramp-smb-get-share (path)
  "Returns the share name of PATH."
  (save-match-data
    (when (string-match "^/?\\([^/]+\\)" path)
      (match-string 1 path))))

(defun tramp-smb-get-path (path)
  "Returns the file name of PATH."
  (save-match-data
    (when (string-match "^/?[^/]+/\\(.*\\)" path)
      (match-string 1 path))))

(defun tramp-smb-get-file-entry (user host share path)
  "Read entry PATH with the `dir' command.
Result is the list (PATH MODE SIZE MONTH DAY TIME YEAR)."
  (save-excursion
    (set-buffer (tramp-get-buffer nil tramp-smb-method user host))
    (tramp-smb-maybe-open-connection user host share)
    (tramp-smb-send-command user host (concat "dir " path))
    (goto-char (point-min))
    (unless (re-search-forward tramp-smb-errors nil t)
      (tramp-smb-read-file-entry))))

(defun tramp-smb-read-file-entry ()
  "Parse entry in SMB output buffer.
Result is the list (PATH MODE SIZE MONTH DAY TIME YEAR)."
  (let ((line (buffer-substring (point) (tramp-point-at-eol))))
    (when (string-match tramp-smb-line line)
      (list
       (match-string 1 line)     ; file name
       (match-string 2 line)     ; permissions
       (match-string 3 line)     ; size
       (match-string 5 line)     ; month
       (match-string 6 line)     ; day
       (match-string 7 line)     ; time
       (match-string 8 line))))) ; year


;; Connection functions

(defun tramp-smb-send-command (user host command)
  "Send the COMMAND to USER at HOST (logged into an SMB session).
Erases temporary buffer before sending the command.
Returns nil in case of unsuccessfull execution."
  (save-excursion
    (set-buffer (tramp-get-buffer nil tramp-smb-method user host))
    (tramp-send-command nil tramp-smb-method user host command nil t)
    (and
     (tramp-smb-wait-for-output user host)
     (save-match-data (not (re-search-forward tramp-smb-errors nil t))))))

(defun tramp-smb-maybe-open-connection (user host share)
  "Maybe open a connection to HOST, logging in as USER, using `tramp-smb-program'.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  (let ((p (get-buffer-process
	    (tramp-get-buffer nil tramp-smb-method user host)))
	last-cmd-time)
    (save-excursion
      (set-buffer (tramp-get-buffer nil tramp-smb-method user host))
      ;; Check whether it is still the same share
      (unless (and p (processp p) (string-equal smb-share share))
	(when (and p (processp p))
	  (delete-process p)
	  (setq p nil)))
      ;; If too much time has passed since last command was sent, look
      ;; whether process is still alive.  If it isn't, kill it.
      (when (and tramp-last-cmd-time
		 (> (tramp-time-diff (current-time) tramp-last-cmd-time) 60)
		 p (processp p) (memq (process-status p) '(run open)))
	(unless (tramp-smb-send-command user host "help")
	  (delete-process p)
	  (setq p nil))
	(erase-buffer)))
    (unless (and p (processp p) (memq (process-status p) '(run open)))
      (when (and p (processp p))
        (delete-process p))
      (tramp-open-connection-smb user host share))))

(defun tramp-open-connection-smb (user host share)
  "Open a connection using `tramp-smb-program'.
This starts the command `smbclient //HOST/SHARE -U USER', then waits
for a remote password prompt.  It queries the user for the password,
then sends the password to the remote host.

If USER is nil, uses value returned by `(user-login-name)' instead."

  (save-match-data
    (let* ((user (or user (user-login-name)))
	   (buffer (tramp-get-buffer nil tramp-smb-method user host)))
      (tramp-pre-connection nil tramp-smb-method user host)
      (tramp-message 7 "Opening connection for //%s@%s/%s..." user host share)
      (let* ((default-directory (tramp-temporary-file-directory))
	     ;; If we omit the conditional here, then we would use
	     ;; `undecided-dos' in some cases.  With the conditional,
	     ;; we use nil in these cases.  Which one is right?
	     (coding-system-for-read (unless (and (not (featurep 'xemacs))
						  (> emacs-major-version 20))
				       tramp-dos-coding-system))
	     (p (funcall 'start-process
			 (buffer-name buffer)
			 buffer
			 tramp-smb-program
			 (concat "//" host "/" share)
			 "-U" user)))
	(process-kill-without-query p)
	(set-buffer buffer)
	(set (make-local-variable 'smb-share) share)

        ; send password
	(let ((pw-prompt "Password:"))
	  (tramp-message 9 "Sending password")
	  (tramp-enter-password p pw-prompt))

	(when (tramp-smb-wait-for-output user host)
	  (erase-buffer))))))

(defun tramp-smb-wait-for-output (user host)
  "Wait for output from smbclient command.
Sets position to begin of buffer.  Returns nil if `tramp-smb-prompt'
is not found."
  (save-excursion
    (let ((proc (get-buffer-process (current-buffer)))
	  (found nil)
	  (start-time (current-time))
	  (timeout 10))
    ;; Algorithm: get waiting output.  See if last line contains
    ;; tramp-smb-prompt sentinel.  If not, wait a bit and again get
    ;; waiting output.  Repeat until timeout expires or tramp-smb-prompt
    ;; sentinel is seen.
      (save-match-data
	;; Work around an XEmacs bug, where the timeout expires
	;; faster than it should.  This degenerates into polling
	;; for buggy XEmacsen, but oh, well.
	(while (and (not found)
		    (< (tramp-time-diff (current-time) start-time)
		       timeout))
	  (with-timeout (timeout)
	    (while (not found)
	      (accept-process-output proc 1)
	      (goto-char (point-max))
	      (beginning-of-line)
	      (setq found (looking-at tramp-smb-prompt))))))
      ;; Add output to debug buffer if appropriate.
      (when tramp-debug-buffer
	(append-to-buffer
	 (tramp-get-debug-buffer nil tramp-smb-method user host)
	 (point-min) (point-max))
	(when (not found)
	  (save-excursion
	    (set-buffer
	     (tramp-get-debug-buffer nil tramp-smb-method user host))
	    (goto-char (point-max))
	    (insert (format "[[Remote prompt `%s' not found in %d secs]]\n"
			    tramp-smb-prompt timeout)))))
      (goto-char (point-min))
      ;; Return value is whether tramp-smb-prompt sentinel was found.
      found)))

(provide 'tramp-smb)

;;; TODO:

;; * Provide a local smb.conf. The default one might not be readable.
;; * Error handling in most of the functions. Brrrr.
;; * Read password from "~/.netrc".
;; * Update documentation.
;; * Provide variables for debug.

;;; tramp-smb.el ends here
