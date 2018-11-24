;;; tramp-sudoedit.el --- Functions for calling emacsclient withfiles under root permissions  -*- lexical-binding:t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes
;; Package: tramp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The "sudoedit" program allows to edit a file as a different user.
;; Exactly this, no more.  Tramp's "sudoedit" method uses this, with
;; emacsclient being the $EDITOR environment variable.
;;
;; Visiting such a remote file in a buffer is implemented with the
;; help of emacsclient.  Therefore, saving this file will save only
;; the temporary file offered by sudoedit.  In order to save the
;; remote file itself, you must leave the visited buffer by the
;; command ´server-edit' ("C-x #").
;;
;; Nothing else will be possible: no file atributes, no file name
;; completion, no file creation/copying, you name it.  The purpose is
;; to make editing such a file as secure as possible; there must be no
;; session running in the Emacs background which could be attacked
;; from inside Emacs.

;;; Code:

(require 'tramp)
(require 'server)

;;;###tramp-autoload
(defconst tramp-sudoedit-method "sudoedit"
  "When this method name is used, call sudoedit for editing a file.")

;;;###tramp-autoload
(defcustom tramp-sudoedit-program "sudoedit"
  "Name of the sudoedit program."
  :group 'tramp
  :version "27.1"
  :type 'string)

;;;###tramp-autoload
(add-to-list 'tramp-methods `(,tramp-sudoedit-method))

;;;###tramp-autoload
(add-to-list 'tramp-default-user-alist '("\\`sudoedit\\'" nil "root"))

;;;###tramp-autoload
(eval-after-load 'tramp
  '(tramp-set-completion-function
    tramp-sudoedit-method tramp-completion-function-alist-su))

(defcustom tramp-sudoedit-emacsclient-waiting-pattern
  (regexp-quote "Waiting for Emacs...")
  "Regular expression matching emacsclient waiting."
  :group 'tramp
  :version "27.1"
  :type 'regexp)

(defconst tramp-sudoedit-actions
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-sudoedit-emacsclient-waiting-pattern tramp-action-succeed)
    (tramp-process-alive-regexp tramp-action-process-alive))
  "List of pattern/action pairs.
This list is used for sudoedit calls.

See `tramp-actions-before-shell' for more info.")

;;;###tramp-autoload
(defconst tramp-sudoedit-file-name-handler-alist
  '((access-file . ignore)
    (add-name-to-file . ignore)
    (byte-compiler-base-file-name . ignore)
    (copy-directory . ignore)
    (copy-file . ignore)
    (delete-directory . ignore)
    (delete-file . ignore)
    (diff-latest-backup-file . ignore)
    ;; `directory-file-name' performed by default handler.
    (directory-files . ignore)
    (directory-files-and-attributes . ignore)
    (dired-compress-file . ignore)
    (dired-uncache . ignore)
    (exec-path . ignore)
    ;; `expand-file-name' performed by default handler.
    (file-accessible-directory-p . ignore)
    (file-acl . ignore)
    (file-attributes . tramp-sudoedit-handle-file-attributes)
    (file-directory-p . ignore)
    (file-equal-p . ignore)
    (file-executable-p . ignore)
    (file-exists-p . ignore)
    (file-in-directory-p . ignore)
    (file-local-copy . ignore)
    (file-modes . ignore)
    (file-name-all-completions . ignore)
    (file-name-as-directory . ignore)
    (file-name-case-insensitive-p . ignore)
    (file-name-completion . ignore)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    (file-name-sans-versions . ignore)
    (file-newer-than-file-p . ignore)
    (file-notify-add-watch . ignore)
    (file-notify-rm-watch . ignore)
    (file-notify-valid-p . ignore)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . ignore)
    (file-regular-p . ignore)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . ignore)
    (file-symlink-p . ignore)
    (file-system-info . ignore)
    (file-truename . identity)
    (file-writable-p . ignore)
    (find-backup-file-name . ignore)
    (get-file-buffer . ignore)
    (insert-directory . ignore)
    (insert-file-contents . tramp-sudoedit-handle-insert-file-contents)
    (load . ignore)
    (make-auto-save-file-name . ignore)
    (make-directory . ignore)
    (make-directory-internal . ignore)
    (make-nearby-temp-file . ignore)
    (make-symbolic-link . ignore)
    (process-file . ignore)
    (rename-file . ignore)
    (set-file-acl . ignore)
    (set-file-modes . ignore)
    (set-file-selinux-context . ignore)
    (set-file-times . ignore)
    (set-visited-file-modtime . ignore)
    (shell-command . ignore)
    (start-file-process . ignore)
    (substitute-in-file-name . identity)
    (temporary-file-directory . ignore)
    (unhandled-file-name-directory . ignore)
    (vc-registered . ignore)
    (verify-visited-file-modtime . ignore)
    (write-region . ignore))
  "Alist of handler functions for Tramp SUDOEDIT method.")

;; It must be a `defsubst' in order to push the whole code into
;; tramp-loaddefs.el.  Otherwise, there would be recursive autoloading.
;;;###tramp-autoload
(defsubst tramp-sudoedit-file-name-p (filename)
  "Check if it's a filename for SUDOEDIT."
  (and (tramp-tramp-file-p filename)
       (string= (tramp-file-name-method (tramp-dissect-file-name filename))
		tramp-sudoedit-method)))

;;;###tramp-autoload
(defun tramp-sudoedit-file-name-handler (operation &rest args)
  "Invoke the SUDOEDIT handler for OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION."
  (let ((fn (assoc operation tramp-sudoedit-file-name-handler-alist)))
    (if fn
	(save-match-data (apply (cdr fn) args))
      (tramp-run-real-handler operation args))))

;;;###tramp-autoload
(tramp-register-foreign-file-name-handler
 'tramp-sudoedit-file-name-p 'tramp-sudoedit-file-name-handler)

(defun tramp-sudoedit-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (file-attributes localname id-format)))

(defvar tramp-current-buffer nil
  "Storage for the current buffer, used in `server-visit-files'")

(defun tramp-sudoedit-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for Tramp files."
  (barf-if-buffer-read-only)

  (with-parsed-tramp-file-name (expand-file-name filename) nil

    ;; Check, whether a server process is already running.  If not,
    ;; start a new one.
    (setq server-name "foo")
    (unless server-process
      (server-force-delete)
      (server-start))

    ;; Advice `server-visit-files'.  It will remove itself after running.
    (setq tramp-current-buffer (current-buffer))
    (advice-add 'server-visit-files :before 'tramp-sudoedit-server-visit-files)

    (with-current-buffer (tramp-get-connection-buffer v)
      (erase-buffer)
      (let* ((process-connection-type tramp-process-connection-type)
	     (command
	      (mapconcat
	       'identity
	       `("env" ,(format "EDITOR='emacsclient -s %s'" server-name)
		 ,tramp-sudoedit-program "-s" "-H" "-p" "Password:" ,localname)
	       " "))
	     (p (start-process-shell-command (tramp-get-connection-name v)
					     (current-buffer) command))
	     ;; We do not want to save the password.
	     auth-source-save-behavior)
	(tramp-message
	 v 6 "%s" (mapconcat 'identity (process-command p) " "))
	(process-put p 'vector v)
	(process-put p 'adjust-window-size-function 'ignore)
	(set-process-query-on-exit-flag p nil)
	(setq tramp-current-connection (cons v (current-time)))
	(tramp-process-actions p v nil tramp-sudoedit-actions))))

  ;; Result.
  (list (expand-file-name filename) (point-max)))

(defun tramp-sudoedit-server-visit-files (files proc &optional nowait)
  "Insert temporary file into the buffer visiting the respective remote file."
  (with-current-buffer tramp-current-buffer
    (insert-file-contents (caar files))
    (setq buffer-file-name (caar files))
    (set-buffer-modified-p nil))
  (tramp-message (car tramp-current-connection) 4 "Inserting %s" (caar files))
  (advice-remove 'server-visit-files 'tramp-sudoedit-server-visit-files))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-sudoedit 'force)))

(provide 'tramp-sudoedit)

;;; tramp-sudoedit.el ends here
