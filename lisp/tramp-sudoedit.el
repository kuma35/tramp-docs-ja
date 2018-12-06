;;; tramp-sudoedit.el --- Functions for calling emacsclient with files under root permissions  -*- lexical-binding:t -*-

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
;; Visiting and writing such a remote file is implemented with the
;; help of `env EDITOR=emacsclient sudoedit'.  The purpose is to make
;; editing such a file as secure as possible; there must be no session
;; running in the Emacs background which could be attacked from inside
;; Emacs.  TRhe default restrictiuon of `sudoedit' apply, like the
;; respective directory must be writable for the given user only,
;; password expiration, etc pp.
;;
;; All other primitive file operations are implemented by a proper
;; `sudo' call.  Again, there will be no shell session running in the
;; Emacs background, every file operation is implemented by its own
;; `sudo' call.

;;; Code:

(require 'tramp)
(require 'server)

;; TODO: REPLACE
(require 'tramp-gvfs)
(require 'eshell)

;;;###tramp-autoload
(defconst tramp-sudoedit-method "sudoedit"
  "When this method name is used, call sudoedit for editing a file.")

;;;###tramp-autoload
(add-to-list 'tramp-methods
  `(,tramp-sudoedit-method
    (tramp-sudo-login     (("sudo") ("-u" "%u") ("-S") ("-H")
			   ("-p" "Password:") ("--")))
    (tramp-sudoedit-login (("env") ("EDITOR=emacsclient")
			   ("EMACS_SOCKET_NAME=%s")
			   ("sudoedit") ("-u" "%u") ("-s") ("-H")
			   ("-p" "Password:")))))

;;;###tramp-autoload
(add-to-list 'tramp-default-user-alist '("\\`sudoedit\\'" nil "root"))

;;;###tramp-autoload
(eval-after-load 'tramp
  '(tramp-set-completion-function
    tramp-sudoedit-method tramp-completion-function-alist-su))

(defconst tramp-sudoedit-emacsclient-pattern
  (regexp-quote "Waiting for Emacs...")
  "Regular expression matching emacsclient waiting.")

(defconst tramp-sudoedit-emacsclient-actions
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-sudoedit-emacsclient-pattern tramp-action-succeed)
    (tramp-process-alive-regexp tramp-action-process-alive))
  "List of pattern/action pairs.
This list is used for sudoedit emacsclient calls.

See `tramp-actions-before-shell' for more info.")

(defconst tramp-sudoedit-sudo-actions
  '((tramp-password-prompt-regexp tramp-action-password)
    (tramp-wrong-passwd-regexp tramp-action-permission-denied)
    (tramp-process-alive-regexp tramp-sudoedit-action-sudo))
  "List of pattern/action pairs.
This list is used for sudoedit emacsclient calls.

See `tramp-actions-before-shell' for more info.")

;;;###tramp-autoload
(defconst tramp-sudoedit-file-name-handler-alist
  '((access-file . ignore)
    (add-name-to-file . ignore)
    (byte-compiler-base-file-name . ignore)
    (copy-directory . ignore)
    (copy-file . tramp-sudoedit-handle-copy-file)
    (delete-directory . ignore)
    (delete-file . tramp-sudoedit-handle-delete-file)
    (diff-latest-backup-file . ignore)
    ;; `directory-file-name' performed by default handler.
    (directory-files . tramp-handle-directory-files)
    (directory-files-and-attributes
     . tramp-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . ignore)
    ;; `expand-file-name' performed by default handler.
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . ignore)
    (file-attributes . tramp-sudoedit-handle-file-attributes)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-sudoedit-handle-file-executable-p)
    (file-exists-p . tramp-sudoedit-handle-file-exists-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-gvfs-handle-file-local-copy)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions
     . tramp-sudoedit-handle-file-name-all-completions)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    ;; `file-name-sans-versions' performed by default handler.
    (file-newer-than-file-p . ignore)
    (file-notify-add-watch . ignore)
    (file-notify-rm-watch . ignore)
    (file-notify-valid-p . ignore)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . tramp-sudoedit-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . ignore)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . ignore)
    (file-truename . identity)
    (file-writable-p . tramp-sudoedit-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-handle-insert-directory)
    (insert-file-contents . tramp-sudoedit-handle-insert-file-contents)
    (load . tramp-handle-load)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . ignore)
    (make-directory-internal . ignore)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-symbolic-link . ignore)
    (process-file . ignore)
    (rename-file . tramp-sudoedit-handle-rename-file)
    (set-file-acl . ignore)
    (set-file-modes . ignore)
    (set-file-selinux-context . ignore)
    (set-file-times . ignore)
    (set-visited-file-modtime . ignore)
    (shell-command . ignore)
    (start-file-process . ignore)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (unhandled-file-name-directory . ignore)
    (vc-registered . ignore)
    (verify-visited-file-modtime . ignore)
    (write-region . tramp-sudoedit-handle-write-region))
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


;; File name primitives.

(defun tramp-sudoedit-do-copy-or-rename-file
  (op filename newname &optional ok-if-already-exists keep-date
   preserve-uid-gid preserve-extended-attributes)
  "Copy or rename a remote file.
OP must be `copy' or `rename' and indicates the operation to perform.
FILENAME specifies the file to copy or rename, NEWNAME is the name of
the new file (for copy) or the new name of the file (for rename).
OK-IF-ALREADY-EXISTS means don't barf if NEWNAME exists already.
KEEP-DATE means to make sure that NEWNAME has the same timestamp
as FILENAME.  PRESERVE-UID-GID, when non-nil, instructs to keep
the uid and gid if both files are on the same host.
PRESERVE-EXTENDED-ATTRIBUTES activates selinux and acl commands.

This function is invoked by `tramp-sudoedit-handle-copy-file' and
`tramp-sudoedit-handle-rename-file'.  It is an error if OP is
neither of `copy' and `rename'.  FILENAME and NEWNAME must be
absolute file names."
  (unless (memq op '(copy rename))
    (error "Unknown operation `%s', must be `copy' or `rename'" op))

  (setq filename (file-truename filename))
  (if (file-directory-p filename)
      (progn
	(copy-directory filename newname keep-date t)
	(when (eq op 'rename) (delete-directory filename 'recursive)))

    (let ((t1 (tramp-tramp-file-p filename))
	  (t2 (tramp-tramp-file-p newname))
	  ;; `file-extended-attributes' exists since Emacs 24.4.
	  (attributes (and preserve-extended-attributes
			   (apply 'file-extended-attributes (list filename))))
	  (sudoedit-operation
	   (cond
	    ((and (eq op 'copy) preserve-uid-gid) '("cp" "-f" "-p"))
	    ((eq op 'copy) '("cp" "-f"))
	    ((eq op 'rename) '("mv" "-f"))))
	  (msg-operation (if (eq op 'copy) "Copying" "Renaming")))

      (with-parsed-tramp-file-name (if t1 filename newname) nil
	(when (and (not ok-if-already-exists) (file-exists-p newname))
	  (tramp-error v 'file-already-exists newname))

	(if (or (and t1 (not (tramp-sudoedit-file-name-p filename)))
		(and t2 (not (tramp-sudoedit-file-name-p newname))))

	    ;; We cannot copy or rename directly.
	    (let ((tmpfile (tramp-compat-make-temp-file filename)))
	      (if (eq op 'copy)
		  (copy-file
		   filename tmpfile t keep-date preserve-uid-gid
		   preserve-extended-attributes)
		(rename-file filename tmpfile t))
	      (rename-file tmpfile newname ok-if-already-exists))

	  ;; Direct action.
	  (with-tramp-progress-reporter
	      v 0 (format "%s %s to %s" msg-operation filename newname)
	    (unless (apply 'tramp-sudoedit-send-command v
			   (append sudoedit-operation
				   `(,(file-local-name filename)
				     ,(file-local-name newname))))
	      (tramp-error
	       v 'file-error
	       "Error %s `%s' `%s'" msg-operation filename newname)))

	  (when (and t1 (eq op 'rename))
	    (with-parsed-tramp-file-name filename v1
	      (tramp-flush-file-properties
	       v1 (file-name-directory v1-localname))
	      (tramp-flush-file-properties v1 v1-localname)))

	  (when t2
	    (with-parsed-tramp-file-name newname v2
	      (tramp-flush-file-properties
	       v2 (file-name-directory v2-localname))
	      (tramp-flush-file-properties v2 v2-localname)
	      (when (tramp-rclone-file-name-p newname)))))))))

(defun tramp-sudoedit-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date
   preserve-uid-gid preserve-extended-attributes)
  "Like `copy-file' for Tramp files."
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  ;; At least one file a Tramp file?
  (if (or (tramp-tramp-file-p filename)
	  (tramp-tramp-file-p newname))
      (tramp-sudoedit-do-copy-or-rename-file
       'copy filename newname ok-if-already-exists keep-date
       preserve-uid-gid preserve-extended-attributes)
    (tramp-run-real-handler
     'copy-file
     (list filename newname ok-if-already-exists keep-date
	   preserve-uid-gid preserve-extended-attributes))))

(defun tramp-sudoedit-handle-delete-file (filename &optional trash)
  "Like `delete-file' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (tramp-flush-file-properties v (file-name-directory localname))
    (tramp-flush-file-properties v localname)
    (unless
	(tramp-sudoedit-send-command
	 v (if (and trash delete-by-moving-to-trash) "trash" "rm")
	 (file-local-name filename))
      ;; Propagate the error.
      (with-current-buffer (tramp-get-connection-buffer v)
	(goto-char (point-min))
	(tramp-error-with-buffer
	 nil v 'file-error "Couldn't delete %s" filename)))))

(defun tramp-sudoedit-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for Tramp files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property
	v localname (format "file-attributes-%s" id-format)
      (tramp-message v 5 "file attributes: %s" localname)
      (ignore-errors
	;; In `tramp-convert-file-attributes', the connection properties
	;; {uig,gid}-{integer,string} are used.  We set them to proper values.
	(with-tramp-connection-property v "uid-integer"
	  (tramp-sudoedit-send-command-and-read v "id" "-u"))
	(with-tramp-connection-property v "gid-integer"
	  (tramp-sudoedit-send-command-and-read v "id" "-g"))
	(with-tramp-connection-property v "uid-string"
	  (symbol-name (tramp-sudoedit-send-command-and-read v "id" "-un")))
	(with-tramp-connection-property v "gid-string"
	  (symbol-name (tramp-sudoedit-send-command-and-read v "id" "-gn")))
	(tramp-convert-file-attributes
	 v
	 (tramp-sudoedit-send-command-and-read
	  v "stat" "-c"
	  (format
	   ;; Apostrophes in the stat output are masked as
	   ;; `tramp-stat-marker', in order to make a proper shell
	   ;; escape of them in file names.
	   "((%s%%N%s) %%h %s %s %%X %%Y %%Z %%s %s%%A%s t %%i -1)"
	   tramp-stat-marker tramp-stat-marker
	   (if (eq id-format 'integer)
	       "%u"
	     (eval-when-compile
	       (concat tramp-stat-marker "%U" tramp-stat-marker)))
	   (if (eq id-format 'integer)
	       "%g"
	     (eval-when-compile
	       (concat tramp-stat-marker "%G" tramp-stat-marker)))
	   tramp-stat-marker tramp-stat-marker)
	  localname))))))
;	  (shell-quote-argument localname)
;	  "|" "sed"
;	  "-e" "'s/\"/\\\\\"/g'"
;	  "-e" (format "'s/%s/\"/g'" tramp-stat-quoted-marker)))))))

(defun tramp-sudoedit-handle-file-executable-p (filename)
  "Like `file-executable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-executable-p"
      (tramp-sudoedit-send-command v "test" "-x" localname))))

(defun tramp-sudoedit-handle-file-exists-p (filename)
  "Like `file-exists-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-exists-p"
      (tramp-sudoedit-send-command v "test" "-e" localname))))

(defun tramp-sudoedit-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for Tramp files."
  (all-completions
   filename
   (with-parsed-tramp-file-name (expand-file-name directory) nil
     (with-tramp-file-property v localname "file-name-all-completions"
       (tramp-sudoedit-send-command
	v "ls" "-a1" (shell-quote-argument localname))
       (mapcar
	(lambda (f)
	  (if (file-directory-p (expand-file-name f directory))
	      (file-name-as-directory f)
	    f))
	(with-current-buffer (tramp-get-connection-buffer v)
	  (delq
	   nil
	   (mapcar
	    (lambda (l) (and (not (string-match-p  "^[[:space:]]*$" l)) l))
	    (split-string (buffer-string) "\n" 'omit)))))))))

(defun tramp-sudoedit-handle-file-readable-p (filename)
  "Like `file-readable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-readable-p"
      (tramp-sudoedit-send-command v "test" "-r" localname))))

(defun tramp-sudoedit-handle-file-writable-p (filename)
  "Like `file-writable-p' for Tramp files."
  (with-parsed-tramp-file-name filename nil
    (with-tramp-file-property v localname "file-writable-p"
      (if (file-exists-p filename)
	  (tramp-sudoedit-send-command v "test" "-w" localname)
	(let ((dir (file-name-directory filename)))
	  (and (file-exists-p dir)
	       (file-writable-p dir)))))))

(defun tramp-sudoedit-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for Tramp files."
  (barf-if-buffer-read-only)
  (setq filename (expand-file-name filename))

  (unwind-protect
      (with-parsed-tramp-file-name filename nil
	;; Set hooks.
	(defalias 'tramp-sudoedit-hook-function
	  `(lambda ()
	     (tramp-sudoedit-insert-file-contents-server-visit-function
	      ',v ,(current-buffer) ,filename ,visit ,beg ,end ,replace)))
	(add-hook 'server-visit-hook 'tramp-sudoedit-hook-function)
	(add-hook 'server-switch-hook 'tramp-sudoedit-server-switch-function)

	;; Call emacsclient.
	(tramp-sudoedit-call-emacsclient v))

    ;; Save exit.
    (when visit
      (setq buffer-file-name filename)
      (setq buffer-read-only (not (file-writable-p filename)))
      (set-visited-file-modtime)
      (set-buffer-modified-p nil)))

  ;; Result.
  (list filename (tramp-compat-file-attribute-size (file-attributes filename))))

(defun tramp-sudoedit-handle-write-region
    (start end filename &optional append visit lockname mustbenew)
  "Like `write-region' for Tramp files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (when (and mustbenew (file-exists-p filename)
	       (or (eq mustbenew 'excl)
		   (not
		    (y-or-n-p
		     (format "File %s exists; overwrite anyway? " filename)))))
      (tramp-error v 'file-already-exists filename))

    ;; Set hooks.
    (defalias 'tramp-sudoedit-hook-function
      `(lambda ()
	 (tramp-sudoedit-write-region-server-visit-function
	  ',v ,(current-buffer)
	  ,start ,end ,filename ,append ,visit ,lockname ,mustbenew)))
    (add-hook 'server-visit-hook 'tramp-sudoedit-hook-function)
    (add-hook 'server-switch-hook 'tramp-sudoedit-server-switch-function)

    ;; Call emacsclient.
    (tramp-sudoedit-call-emacsclient v)

    (tramp-flush-file-properties v (file-name-directory localname))
    (tramp-flush-file-properties v localname)

    ;; Set file modification time.
    (when (or (eq visit t) (stringp visit))
      (set-visited-file-modtime
       (tramp-compat-file-attribute-modification-time
	(file-attributes filename))))

    ;; The end.
    (when (and (null noninteractive)
	       (or (eq visit t) (null visit) (stringp visit)))
      (tramp-message v 0 "Wrote %s" filename))
    (run-hooks 'tramp-handle-write-region-hook)))


;; Internal functions.

;; Used in `tramp-sudoedit-sudo-actions'.
(defun tramp-sudoedit-action-sudo (proc vec)
  "Check, whether a sudo process copy has finished."
  ;; There might be pending output for the exit status.
  (tramp-accept-process-output proc 0.1)
  (when (not (process-live-p proc))
    ;; Delete narrowed region, it would be in the way reading a Lisp form.
    (goto-char (point-min))
    (widen)
    (delete-region (point-min) (point))
    (tramp-message vec 3 "Process has finished.")
    (throw 'tramp-action 'ok)))

(defun tramp-sudoedit-send-command (vec command &rest args)
  "Send the COMMAND to connection VEC.
Erases temporary buffer before sending the command.  Erases nil
in case of error, t otherwise."
  (with-current-buffer (tramp-get-connection-buffer vec)
    (erase-buffer)
    ;; Sudo does accepts only commands, no pipes and alike.
    (dolist (elt (cons command args))
      (when (and (stringp elt) (string-match "|\\|&\\;" elt))
	(tramp-error
	 vec 'file-error "Shell construct %s not allowed in `%s'"
	 (match-string 0 elt)
	 (mapconcat 'identity (cons command args) " "))))
    (let* ((login (tramp-get-method-parameter vec 'tramp-sudo-login))
	   (host (or (tramp-file-name-host vec) ""))
	   (user (or (tramp-file-name-user vec) ""))
	   (spec (format-spec-make ?h host ?u user))
	   (args (append
		  ;; TODO: Replace.
		  (eshell-flatten-list
		   (mapcar
		    (lambda (x)
		      (setq x (mapcar (lambda (y) (format-spec y spec)) x))
		      (unless (member "" x) x))
		    login))
		  `(,command) args))
	   (process-connection-type tramp-process-connection-type)
	   (p (apply 'start-process
		     (tramp-get-connection-name vec) (current-buffer) args))
	   ;; We suppress the messages `Waiting for prompts from remote shell'.
	   (tramp-verbose (if (= tramp-verbose 3) 2 tramp-verbose))
	   ;; We do not want to save the password.
	   auth-source-save-behavior)
      (tramp-message vec 6 "%s" (mapconcat 'identity (process-command p) " "))
      (process-put p 'vector vec)
      (process-put p 'adjust-window-size-function 'ignore)
      (set-process-query-on-exit-flag p nil)
      (tramp-process-actions p vec nil tramp-sudoedit-sudo-actions)
      ;; Delete process finished message.
      (goto-char (point-min))
      (when (re-search-forward
	     (format "Process %s" (regexp-quote (process-name p))) nil t)
	(delete-region (match-beginning 0) (point-max)))
      (tramp-message vec 6 "%s\n%s" (process-exit-status p) (buffer-string))
      (zerop (process-exit-status p)))))

(defun tramp-sudoedit-send-command-and-read (vec command &rest args)
  "Run COMMAND and return the output, which must be a Lisp expression.
In case there is no valid Lisp expression and NOERROR is nil, it
raises an error."
  (when (apply 'tramp-sudoedit-send-command vec command args)
    (with-current-buffer (tramp-get-connection-buffer vec)
      ;; Replace stat marker.
      (goto-char (point-min))
      (while (search-forward tramp-stat-marker nil t)
	(replace-match "\""))
      ;; Read the expression.
      (tramp-message vec 6 "\n%s" (buffer-string))
      (goto-char (point-min))
      (condition-case nil
	  (prog1 (read (current-buffer))
	    ;; Error handling.
	    (when (re-search-forward "\\S-" (point-at-eol) t)
	      (error nil)))
	(error (unless t ;; TODO: noerror must be handled. somehow.
		 (tramp-error
		  vec 'file-error
		  "`%s' does not return a valid Lisp expression: `%s'"
		  command (buffer-string))))))))

(defun tramp-sudoedit-call-emacsclient (vec)
  "Call sudoedit with EDITOR=emacsclient."
  ;; Check, whether a server process is already running.  If not,
  ;; start a new one.
  (setq server-name "foo")
  (unless server-process
    (server-force-delete)
    (server-start))

  ;; Call emacsclient.
  (with-current-buffer (tramp-get-connection-buffer vec)
    (erase-buffer)
    (let* ((login (tramp-get-method-parameter vec 'tramp-sudoedit-login))
	   (host (or (tramp-file-name-host vec) ""))
	   (user (or (tramp-file-name-user vec) ""))
	   (spec (format-spec-make ?h host ?u user ?s server-name))
	   (args (append
		  ;; TODO: Replace.
		  (eshell-flatten-list
		   (mapcar
		    (lambda (x)
		      (setq x (mapcar (lambda (y) (format-spec y spec)) x))
		      (unless (member "" x) x))
		    login))
		  `(,(tramp-file-name-localname vec))))
	   (process-connection-type tramp-process-connection-type)
	   (p (apply 'start-process
		     (tramp-get-connection-name vec) (current-buffer) args))
	   ;; We suppress the messages `Waiting for prompts from remote shell'.
	   (tramp-verbose (if (= tramp-verbose 3) 2 tramp-verbose))
	   ;; We do not want to save the password.
	   auth-source-save-behavior)
      (tramp-message vec 6 "%s" (mapconcat 'identity (process-command p) " "))
      (process-put p 'vector vec)
      (process-put p 'adjust-window-size-function 'ignore)
      (set-process-query-on-exit-flag p nil)
      (tramp-process-actions p vec nil tramp-sudoedit-emacsclient-actions))))

(defun tramp-sudoedit-insert-file-contents-server-visit-function
  (vec buffer _filename &optional _visit beg end _replace)
  "Insert temporary file into the buffer visiting the respective remote file.
VEC is the current Tramp connection, BUFFER is the current buffer
when adding the function to the hook."
  (unwind-protect
      (let ((curbuf (current-buffer)))
	(tramp-message vec 4 "Inserting %s" (buffer-file-name))
	(with-current-buffer buffer
	  (let (buffer-read-only)
	    (erase-buffer)
	    (insert-buffer-substring curbuf beg end)
	    (set-buffer-modified-p nil))))
    ;; Cleanup.
    (remove-hook 'server-visit-hook
		 'tramp-sudoedit-insert-file-contents-server-visit-function)))

(defun tramp-sudoedit-handle-rename-file
  (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for Tramp files."
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  ;; At least one file a Tramp file?
  (if (or (tramp-tramp-file-p filename)
          (tramp-tramp-file-p newname))
      (tramp-sudoedit-do-copy-or-rename-file
       'rename filename newname ok-if-already-exists
       'keep-date 'preserve-uid-gid)
    (tramp-run-real-handler
     'rename-file (list filename newname ok-if-already-exists))))

(defun tramp-sudoedit-write-region-server-visit-function
   (vec buffer start end _filename &optional append _visit _lockname _mustbenew)
  "Write currect buffer into the respective remote file.
VEC is the current Tramp connection, BUFFER is the current buffer
when adding the function to the hook."
  (unwind-protect
      (let ((currfile (buffer-file-name)))
	(tramp-message vec 4 "Writing %s" (buffer-file-name))
	;; We say `no-message' here because we don't want the visited
	;; file modtime data to be clobbered from the temp file.  We
	;; call `set-visited-file-modtime' ourselves later on.
	(with-current-buffer buffer
	  (tramp-run-real-handler
	   'write-region
	   (list start end currfile append 'no-message))))
    ;; Cleanup.
    (remove-hook 'server-visit-hook
		 'tramp-sudoedit-hook-function)))

(defun tramp-sudoedit-server-switch-function ()
  "Kill temporary file of emacsclient."
  (unwind-protect
      (progn
	(setq file-name-history (delete (buffer-file-name) file-name-history))
	(server-edit))
    (remove-hook 'server-switch-hook 'tramp-sudoedit-server-switch-function)))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-sudoedit 'force)))

(provide 'tramp-sudoedit)

;;; tramp-sudoedit.el ends here
