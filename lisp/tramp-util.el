;;; -*- coding: iso-2022-7bit; -*-
;;; tramp-util.el --- Misc utility functions to use with Tramp

;; Copyright (C) 2001, 2002, 2003, 2004, 2005,
;;   2006 Free Software Foundation, Inc.

;; Author: Kai Gro,A_(Bjohann <kai.grossjohann@gmx.net>
;;         Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, extensions, processes

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Some misc. utility functions that might go nicely with Tramp.
;; Mostly, these are kluges awaiting real solutions later on.

;;; Code:

(eval-when-compile (require 'cl))
(require 'tramp)

;; Define a Tramp minor mode. It's intention is to redefine some keys
;; for Tramp specific functions, like compilation.

(defvar tramp-minor-mode-map (make-sparse-keymap)
  "Keymap for Tramp minor mode.")

(define-minor-mode tramp-minor-mode "Tramp minor mode for utility functions."
  :group 'tramp
  :global nil
  :init-value nil
  :lighter " Tramp"
  :keymap tramp-minor-mode-map
  (setq tramp-minor-mode
	(and tramp-minor-mode (tramp-tramp-file-p default-directory))))

(add-hook 'find-file-hooks 'tramp-minor-mode t)
(add-hook 'tramp-util-unload-hook
	  '(lambda ()
	     (remove-hook 'find-file-hooks 'tramp-minor-mode)))

(add-hook 'dired-mode-hook 'tramp-minor-mode t)
(add-hook 'tramp-util-unload-hook
	  '(lambda ()
	     (remove-hook 'dired-mode-hook 'tramp-minor-mode)))

(defun tramp-remap-command (old-command new-command)
  "Replaces bindings of OLD-COMMAND by NEW-COMMAND.
If remapping functionality for keymaps is defined, this happens for all
bindings.  Otherwise, only bindings active during invocation are taken
into account.  XEmacs menubar bindings are not changed by this."
  (if (functionp 'command-remapping)
      ;; Emacs 22
      (eval
       `(define-key tramp-minor-mode-map [remap ,old-command] new-command))
    ;; previous Emacs versions.
    (mapcar
     '(lambda (x)
	(define-key tramp-minor-mode-map x new-command))
     (where-is-internal old-command))))


;; Utility functions.

;; `executable-find', `start-process' and `call-process' have no file
;; handler yet.  The idea is that such a file handler is called when
;; `default-directory' matches a regexp in `file-name-handler-alist'.
;; This would allow to run commands on remote hosts.  The disadvantage
;; is, that commands which should run locally anyway, would also run
;; remotely, like the commands called by `gnus'.  This implementation
;; is an experimental one as proof of concept, it will change after
;; iscussion with (X)Emacs maintainers.

;; In Emacs 22, there is already `process-file', which is similar to
;; `call-process'.

;; `start-process-shell-command' and `call-process-shell-command' must
;; be advised, because they use `shell-file-name'.  It cannot be
;; assumed that the shell on a remote host is equal to the one of the
;; local host.

;; `call-process-on-region' does not work (yet) this way, it needs mor
;; investigation.  The same is true for synchronous `shell-command',
;; which applies `call-process-on-region' internally.

;; Other open problems are `setenv'/`getenv'.

(unless (tramp-exists-file-name-handler 'executable-find "ls")
  (defadvice executable-find
    (around tramp-advice-executable-find activate)
    "Invoke `tramp-handle-executable-find' for Tramp files."
    (if (eq (tramp-find-foreign-file-name-handler default-directory)
	    'tramp-sh-file-name-handler)
	(setq ad-return-value
	      (apply 'tramp-handle-executable-find (ad-get-args 0)))
      ad-do-it))
  (add-hook 'tramp-util-unload-hook
	    '(lambda () (ad-unadvise 'executable-find))))

(unless (tramp-exists-file-name-handler 'start-process "" nil "ls")
  (defadvice start-process
    (around tramp-advice-start-process activate)
    "Invoke `tramp-handle-start-process' for Tramp files."
    (if (eq (tramp-find-foreign-file-name-handler default-directory)
	    'tramp-sh-file-name-handler)
	(setq ad-return-value
	      (apply 'tramp-handle-start-process (ad-get-args 0)))
      ad-do-it))
  (add-hook 'tramp-util-unload-hook
	    '(lambda () (ad-unadvise 'start-process))))

(unless (tramp-exists-file-name-handler
	 'start-process-shell-command "" nil "ls")
  (defadvice start-process-shell-command
    (around tramp-advice-start-process-shell-command activate)
    "Invoke `tramp-handle-start-process-shell-command' for Tramp files."
    (if (eq (tramp-find-foreign-file-name-handler default-directory)
	    'tramp-sh-file-name-handler)
	(with-parsed-tramp-file-name default-directory nil
	  (let ((shell-file-name
		 (tramp-get-connection-property v "remote-shell" nil)))
	    ad-do-it))
      ad-do-it))
  (add-hook 'tramp-util-unload-hook
	    '(lambda () (ad-unadvise 'start-process-shell-command))))

(unless (tramp-exists-file-name-handler 'call-process "ls")
  (defadvice call-process
    (around tramp-advice-call-process activate)
    "Invoke `tramp-handle-call-process' for Tramp files."
    (if (eq (tramp-find-foreign-file-name-handler default-directory)
	    'tramp-sh-file-name-handler)
	(setq ad-return-value
	      (apply 'tramp-handle-call-process (ad-get-args 0)))
      ad-do-it))
  (add-hook 'tramp-util-unload-hook
	    '(lambda () (ad-unadvise 'call-process))))

(unless (tramp-exists-file-name-handler 'call-process-shell-command "ls")
  (defadvice call-process-shell-command
    (around tramp-advice-call-process-shell-command activate)
    "Invoke `tramp-handle-call-process-shell-command' for Tramp files."
    (if (eq (tramp-find-foreign-file-name-handler default-directory)
	    'tramp-sh-file-name-handler)
	(with-parsed-tramp-file-name default-directory nil
	  (let ((shell-file-name
		 (tramp-get-connection-property v "remote-shell" nil)))
	    ad-do-it))
      ad-do-it))
  (add-hook 'tramp-util-unload-hook
	    '(lambda () (ad-unadvise 'call-process-shell-command))))

(if (not (fboundp 'file-remote-p))
    ;; Emacs 21
    (defalias 'file-remote-p (symbol-function 'tramp-handle-file-remote-p))
  (unless (tramp-exists-file-name-handler 'file-remote-p "/")
    ;; XEmacs 21
    (defadvice file-remote-p
      (around tramp-advice-file-remote-p (filename) activate)
      "Invoke `tramp-handle-file-remote-p' for Tramp files."
      (if (eq (tramp-find-foreign-file-name-handler (expand-file-name filename))
	      'tramp-sh-file-name-handler)
	  (setq ad-return-value
		(tramp-handle-file-remote-p filename))
	ad-do-it))
    (add-hook 'tramp-util-unload-hook
	      '(lambda () (ad-unadvise 'file-remote-p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In Emacs 21, `dired-insert-directory' calls
;; `dired-free-space-program'.  This isn't aware of remote
;; directories.  It will be disabled temporarily.

(when (and (boundp 'dired-free-space-program)
	   (not (boundp 'directory-free-space-program))) ;; Alias in Emacs 22.
  (defadvice dired-insert-directory
    ;; Don't use ARGS from `dired-insert-directory', they are
    ;; different for (X)Emacs flavors.
    (around tramp-advice-dired-insert-directory activate)
    "Disable `dired-free-space-program' for Tramp files."
    (let ((dired-free-space-program
	   (and (not
		 (eq (tramp-find-foreign-file-name-handler default-directory)
		     'tramp-sh-file-name-handler))
		(symbol-value 'dired-free-space-program))))
      ad-do-it))
  (add-hook 'tramp-util-unload-hook
	    '(lambda () (ad-unadvise 'dired-insert-directory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compile.el parses the compilation output for file names.  It
;; expects them on the local machine.  This must be changed.

(add-hook
 'compilation-mode-hook
 '(lambda ()
    (set (make-local-variable 'comint-file-name-prefix)
	 (or (file-remote-p default-directory) ""))))

;; `grep-compute-defaults' computes `grep-command',
;; `grep-find-command', `grep-tree-command', `grep-use-null-device',
;; `grep-find-use-xargs' and `grep-highlight-matches'.  Since those
;; values might be different for remote hosts, we set the variables to
;; the "Not Set" (default) value.

(defun tramp-grep-setup ()

  (mapcar
   '(lambda (x)

      (eval
       `(defadvice ,x
	  (around ,(intern (format "tramp-advice-%s" x)) activate)
	  "Make customization variables local for Tramp files."
	  (if (eq (tramp-find-foreign-file-name-handler default-directory)
		  'tramp-sh-file-name-handler)
	      (with-parsed-tramp-file-name default-directory nil
		;; Initialize with default values from defcustom, if
		;; not set yet for this remote connection.
		(let ((grep-command
		       (when (boundp 'grep-command)
			 (tramp-get-connection-property
			  v "grep-command"
			  (eval (car (get 'grep-command
					  'standard-value))))))
		      (grep-find-command
		       (when (boundp 'grep-find-command)
			 (tramp-get-connection-property
			  v "grep-find-command"
			  (eval (car (get 'grep-find-command
					  'standard-value))))))
		      (grep-tree-command
		       (when (boundp 'grep-tree-command)
			 (tramp-get-connection-property
			  v "grep-tree-command"
			  (eval (car (get 'grep-tree-command
					  'standard-value))))))
		      (grep-use-null-device
		       (when (boundp 'grep-use-null-device)
			 (tramp-get-connection-property
			  v "grep-use-null-device"
			  (eval (car (get 'grep-use-null-device
					  'standard-value))))))
		      (grep-find-use-xargs
		       (when (boundp 'grep-find-use-xargs)
			 (tramp-get-connection-property
			  v "grep-find-use-xargs"
			  (eval (car (get 'grep-find-use-xargs
					  'standard-value))))))
		      (grep-highlight-matches
		       (when (boundp 'grep-highlight-matches)
			 (tramp-get-connection-property
			  v "grep-highlight-matches"
			  (eval (car (get 'grep-highlight-matches
					  'standard-value)))))))
		  ad-do-it))
	    ;; local file
	    ad-do-it)))

      (eval
       `(add-hook
	 'tramp-util-unload-hook
	 '(lambda () (ad-unadvise ,x)))))

   '(grep grep-find grep-tree grep-process-setup))

  (defadvice grep-compute-defaults
    (around tramp-advice-grep-compute-defaults activate)
    "Save customization variables for Tramp files."
    (if (eq (tramp-find-foreign-file-name-handler default-directory)
	    'tramp-sh-file-name-handler)
	(with-parsed-tramp-file-name default-directory nil
	  (prog1
	      ad-do-it
	    ;; Save computed values for next run.
	    (when (boundp 'grep-command)
	      (tramp-set-connection-property
	       v "grep-command" (symbol-value 'grep-command)))
	    (when (boundp 'grep-find-command)
	      (tramp-set-connection-property
	       v "grep-find-command" (symbol-value 'grep-find-command)))
	    (when (boundp 'grep-tree-command)
	      (tramp-set-connection-property
	       v "grep-tree-command" (symbol-value 'grep-tree-command)))
	    (when (boundp 'grep-use-null-device)
	      (tramp-set-connection-property
	       v "grep-use-null-device" (symbol-value 'grep-use-null-device)))
	    (when (boundp 'grep-find-use-xargs)
	      (tramp-set-connection-property
	       v "grep-find-use-xargs" (symbol-value 'grep-find-use-xargs)))
	    (when (boundp 'grep-highlight-matches)
	      (tramp-set-connection-property
	       v "grep-highlight-matches"
	       (symbol-value 'grep-highlight-matches)))))
      ;; local file
      ad-do-it))

  (add-hook 'tramp-util-unload-hook
	    '(lambda () (ad-unadvise 'grep-compute-defaults))))

;; Emacs 22 offers it in grep.el, (X)Emacs 21 in compile.el.
(if (locate-library "grep")
    (eval-after-load "grep" '(tramp-grep-setup))
  (eval-after-load "compile" '(tramp-grep-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gud.el uses `gud-find-file' for specifying a file name function.
;; In XEmacs 21, 'gud must be required before calling `gdb'.
;; Otherwise, gdb.el is used, which is not supported.

(defun tramp-gud-file-name (filename)
  "Evaluate a file name to be loaded.
If it is an absolute file name, and not a remote one, prepend the remote part."
  (let ((filename (expand-file-name filename)))
    (setq filename
	  (if (file-remote-p filename)
	      ;; It is already expanded.
	      filename
	    ;; Prefix the Tramp remote file name.
	    (concat (file-remote-p default-directory) filename)))

    ;; Emacs 22 uses `gud-file-name' which we should do as well.
    ;; `gud-<MINOR-MODE>-directories' must be Tramp file names.
    (if (functionp 'gud-file-name)
	(funcall 'gud-file-name filename)
      filename)))

(defun tramp-gud-massage-args (args)
  "Set arguments of debugger on remote hosts.
They must be changed to be relative to the default directory.
Works only for relative file names and Tramp file names."
  (let ((default-directory (expand-file-name default-directory))
	(item args)
	file)
    (while (car item)
      ;; The expansion is performed for EVERY parameter, even for
      ;; non file names.  But this doesn't hurt, because it is
      ;; changed back to its original value afterwards.
      (setq file (expand-file-name (car item)))
      (when (string-lessp default-directory file)
	(setcar item (substring file (length default-directory))))
      (setq item (cdr item))))
  args)

(defun tramp-gud-setup ()
  (when (functionp 'gud-find-file)
    (set 'gud-find-file 'tramp-gud-file-name))

  (mapcar
   '(lambda (x)

      ;; (X)Emacs 21 use `gud-<MINOR-MODE>-find-file'.
      (eval
       `(defadvice ,(intern (format "gud-%s-find-file" x))
	  (before
	   ,(intern (format "tramp-advice-gud-%s-find-file" x))
	   (filename) activate)
	  "Invoke `tramp-gud-find-file' for Tramp files."
	  (when (eq (tramp-find-foreign-file-name-handler default-directory)
		    'tramp-sh-file-name-handler)
	    (ad-set-arg 0 (tramp-gud-file-name (ad-get-arg 0))))))
      (eval
       `(add-hook
	 'tramp-util-unload-hook
	 '(lambda () (ad-unadvise ,(intern (format "gud-%s-find-file" x))))))

      ;; Arguments shall be trimmed to local file names.
      (eval
       `(defadvice ,(intern (format "gud-%s-massage-args" x))
	  (before
	   ,(intern (format "tramp-advice-gud-%s-massage-args" x))
	   (file args) activate)
	  "Invoke `tramp-gud-massage-args' for Tramp files."
	  (when (eq (tramp-find-foreign-file-name-handler
		     (expand-file-name (ad-get-arg 0)))
		    'tramp-sh-file-name-handler)
	    (ad-set-arg 0 (car (tramp-gud-massage-args (list (ad-get-arg 0)))))
	    (ad-set-arg 1 (tramp-gud-massage-args (ad-get-arg 1))))))
      (eval
       `(add-hook
	 'tramp-util-unload-hook
	 '(lambda () (ad-unadvise
		      ,(intern (format "gud-%s-massage-args" x)))))))

   ;; So far, I've tested only gdb and perldb.
   ;; (X)Emacs
   '(gdb sdb dbx xdb perldb
   ;; Emacs
     pdb jdb
   ;; Emacs 22
     bashdb)))

(eval-after-load "gud" '(tramp-gud-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ediff.el must apply its diff command locally.

(defun tramp-ediff-setup ()
  (defadvice ediff-exec-process
    (around tramp-advice-ediff-exec-process activate)
    "Run ediff commands on a local default directory."
    (let ((default-directory (tramp-temporary-file-directory)))
      ad-do-it)))
(add-hook 'tramp-util-unload-hook
	  '(lambda () (ad-unadvise 'ediff-exec-process)))

(eval-after-load "ediff" '(tramp-ediff-setup))

(provide 'tramp-util)

;;; arch-tag: 500f9992-a44e-46d0-83a7-980799251808
;;; tramp-util.el ends here
