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

(defvar tramp-use-smb nil
  "*Temporary variable to pretend SMB application as long as under development.
Change it only if you know what you do.")

;; Define SMB method ...
(defcustom tramp-smb-method "smb"
  "*Method to connect SAMBA and M$ SMB servers."
  :group 'tramp
  :type 'string)

;; ... and add it to the method list.
(add-to-list 'tramp-methods (cons tramp-smb-method nil))

;; Add completion function for SMB method.
(unless (memq system-type '(windows-nt))
  (tramp-set-completion-function
   tramp-smb-method
   '((tramp-parse-netrc "~/.netrc"))))

(defcustom tramp-smb-program "smbclient"
  "*Name of SMB client to run."
  :group 'tramp
  :type 'string)

(defconst tramp-smb-prompt "smb: \\> "
  "String used as prompt in smbclient.")

;; New handlers should be added here.
(defconst tramp-smb-file-name-handler-alist
  '(
;    (load . tramp-handle-load)
;    (make-symbolic-link . tramp-handle-make-symbolic-link)
;    (file-name-directory . tramp-handle-file-name-directory)
;    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
;    (file-truename . tramp-handle-file-truename)
    (file-exists-p . tramp-smb-handle-file-exists-p)
;    (file-directory-p . tramp-handle-file-directory-p)
;    (file-executable-p . tramp-handle-file-executable-p)
;    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
;    (file-readable-p . tramp-handle-file-readable-p)
;    (file-regular-p . tramp-handle-file-regular-p)
;    (file-symlink-p . tramp-handle-file-symlink-p)
;    (file-writable-p . tramp-handle-file-writable-p)
;    (file-ownership-preserved-p . tramp-handle-file-ownership-preserved-p)
;    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
;    (file-attributes . tramp-handle-file-attributes)
;    (file-modes . tramp-handle-file-modes)
;    (file-directory-files . tramp-handle-file-directory-files)
;    (directory-files . tramp-handle-directory-files)
;    (file-name-all-completions . tramp-handle-file-name-all-completions)
;    (file-name-completion . tramp-handle-file-name-completion)
;    (add-name-to-file . tramp-handle-add-name-to-file)
;    (copy-file . tramp-handle-copy-file)
;    (rename-file . tramp-handle-rename-file)
;    (set-file-modes . tramp-handle-set-file-modes)
;    (make-directory . tramp-handle-make-directory)
;    (delete-directory . tramp-handle-delete-directory)
;    (delete-file . tramp-handle-delete-file)
;    (directory-file-name . tramp-handle-directory-file-name)
;    (shell-command . tramp-handle-shell-command)
;    (insert-directory . tramp-handle-insert-directory)
;    (expand-file-name . tramp-handle-expand-file-name)
;    (file-local-copy . tramp-handle-file-local-copy)
;    (insert-file-contents . tramp-handle-insert-file-contents)
;    (write-region . tramp-handle-write-region)
;    (unhandled-file-name-directory . tramp-handle-unhandled-file-name-directory)
;    (dired-call-process . tramp-handle-dired-call-process)
;    (dired-recursive-delete-directory
;     . tramp-handle-dired-recursive-delete-directory)
;    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
;    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime))
)        "Alist of handler functions.
Operations not mentioned here will be handled by the normal Emacs functions.")

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
  ; temporarily disable SMB as long as under development
  (unless tramp-use-smb (error "SMB method not supported yet"))
  (let ((fn (assoc operation tramp-smb-file-name-handler-alist)))
    (if fn
	(save-match-data (apply (cdr fn) args))
      (tramp-run-real-handler operation args))))

(add-to-list 'tramp-foreign-file-name-handler-alist
	     (cons 'tramp-smb-file-name-p 'tramp-smb-file-name-handler))

;; File name primitives

(defun tramp-smb-handle-file-exists-p (filename)
  "Like `file-exists-p' for tramp files."
  (with-parsed-tramp-file-name filename nil
    (save-excursion
      (let* ((share (tramp-smb-get-share path))
	     (file  (tramp-smb-get-path  path))
	     (entry (tramp-smb-get-file-entry user host share file)))
	; check result
	(string-equal (car entry) (file-name-nondirectory file))))))

;; Internal file name functions

(defun tramp-smb-get-share (path)
  "Returns the share name of PATH."
  (save-match-data
    (string-match "^/?\\([^/]+\\)" path)
    (match-string 1 path)))

(defun tramp-smb-get-path (path)
  "Returns the file name of PATH."
  (save-match-data
    (string-match "^/?[^/]+/\\(.*\\)" path)
    (match-string 1 path)))

(defun tramp-smb-get-file-entry (user host share path)
  "Read entry PATH with the `dir' command.
Result is the list (PATH MODE SIZE DATE)."
  (tramp-smb-maybe-open-connection user host share)
  (tramp-smb-send-command user host (concat "dir " path))
  (tramp-smb-wait-for-output 10)
  (goto-char (point-min))
  (unless (re-search-forward "ERRDOS" nil t)
    (let ((line (buffer-substring (point-min) (tramp-point-at-eol))))
      (string-match
       (concat "^[ \t]+\\([^ \t]+\\)"
	        "[ \t]+\\([^ \t]+\\)"
	        "[ \t]+\\([^ \t]+\\)"
	        "[ \t]+\\(.+\\)[ \t]*$")
       line)
      (list (match-string 1 line) (match-string 2 line)
	    (match-string 3 line) (match-string 4 line)))))


;; Connection functions

(defun tramp-smb-send-command (user host command)
  "Send the COMMAND to USER at HOST (logged into an SMB session).
Erases temporary buffer before sending the command."
  (tramp-send-command nil "smb" user host command nil t))

(defun tramp-smb-maybe-open-connection (user host share)
  "Maybe open a connection to HOST, logging in as USER, using `tramp-smb-program'.
Does not do anything if a connection is already open, but re-opens the
connection if a previous connection has died for some reason."
  (let ((p (get-buffer-process (tramp-get-buffer nil "smb" user host)))
	last-cmd-time)
    ;; If too much time has passed since last command was sent, look
    ;; whether process is still alive.  If it isn't, kill it.  When
    ;; using ssh, it can sometimes happen that the remote end has hung
    ;; up but the local ssh client doesn't recognize this until it
    ;; tries to send some data to the remote end.  So that's why we
    ;; try to send a command from time to time, then look again
    ;; whether the process is really alive.
    (save-excursion
      (set-buffer (tramp-get-buffer nil "smb" user host))
      (when (and tramp-last-cmd-time
		 (> (tramp-time-diff (current-time) tramp-last-cmd-time) 60)
		 p (processp p) (memq (process-status p) '(run open)))
	(tramp-smb-send-command user host "help")
	(unless (tramp-smb-wait-for-output 10)
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
    (tramp-pre-connection nil "smb" user host)
    (tramp-message 7 "Opening connection for //%s@%s/%s..." 
		   (or user (user-login-name)) host share)
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "TERM" tramp-terminal-type)
      (let* ((default-directory (tramp-temporary-file-directory))
	     ;; If we omit the conditional here, then we would use
	     ;; `undecided-dos' in some cases.  With the conditional,
	     ;; we use nil in these cases.  Which one is right?
             (coding-system-for-read (unless (and (not (featurep 'xemacs))
                                                  (> emacs-major-version 20))
                                       tramp-dos-coding-system))
             (p (funcall 'start-process
			 (tramp-buffer-name nil "smb" user host)
			 (tramp-get-buffer nil "smb" user host)
			 tramp-smb-program
			 (concat "//" host "/" share)
			 "-U" user)))
        (process-kill-without-query p)
	(set-buffer (tramp-get-buffer nil "smb" user host))

        ; send password
	(let ((pw-prompt "Password:"))
	  (tramp-message 9 "Sending password")
	  (tramp-enter-password p pw-prompt))

	(tramp-smb-wait-for-output 10))))
	(erase-buffer))

(defun tramp-smb-wait-for-output (&optional timeout)
  "Wait for output from remote smbclient command."
  (let ((proc (get-buffer-process (current-buffer)))
        (found nil)
        (start-time (current-time))
        (end-of-output (concat "^" (regexp-quote tramp-smb-prompt))))
    ;; Algorithm: get waiting output.  See if last line contains
    ;; end-of-output sentinel.  If not, wait a bit and again get
    ;; waiting output.  Repeat until timeout expires or end-of-output
    ;; sentinel is seen.  Will hang if timeout is nil and
    ;; end-of-output sentinel never appears.
    (save-match-data
      (cond (timeout
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
                   (setq found (looking-at end-of-output))))))
            (t
             (while (not found)
               (accept-process-output proc 1)
               (goto-char (point-max))
               (forward-line -1)
               (setq found (looking-at end-of-output))))))
    ;; Add output to debug buffer if appropriate.
    (when tramp-debug-buffer
      (append-to-buffer
       (tramp-get-debug-buffer tramp-current-multi-method tramp-current-method
                             tramp-current-user tramp-current-host)
       (point-min) (point-max))
      (when (not found)
        (save-excursion
          (set-buffer
           (tramp-get-debug-buffer tramp-current-multi-method tramp-current-method
                                 tramp-current-user tramp-current-host))
          (goto-char (point-max))
          (insert "[[Remote prompt `" end-of-output "' not found"
                  (if timeout (format " in %d secs" timeout) "")
                  "]]"))))
    (goto-char (point-min))
    ;; Return value is whether end-of-output sentinel was found.
    found))

(provide 'tramp-smb)

;;; TODO:

;; * Provide a local smb.conf. The default one might not be readable

;;; tramp-smb.el ends here
