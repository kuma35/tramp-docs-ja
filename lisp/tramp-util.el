;;; -*- coding: iso-2022-7bit; -*-
;;; tramp-util.el --- Misc utility functions to use with Tramp

;; Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

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
(add-hook 'dired-mode-hook 'tramp-minor-mode t)

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

(unless (tramp-exists-file-name-handler 'start-process "" nil "ls")
  (defadvice start-process
    (around tramp-advice-start-process
	    (name buffer program &rest args)
	    activate)
    "Invoke `tramp-handle-start-process' for Tramp files."
    (if (and default-directory (tramp-tramp-file-p default-directory))
	(setq ad-return-value
	      (apply 'tramp-handle-start-process name buffer program args))
      ad-do-it)))

(unless (tramp-exists-file-name-handler 'call-process "ls")
  (defadvice call-process
    (around tramp-advice-call-process
	    (program &optional infile buffer display &rest args)
	    activate)
    "Invoke `tramp-handle-call-process' for Tramp files."
    (if (and default-directory (tramp-tramp-file-p default-directory))
	(setq ad-return-value
	      (apply 'tramp-handle-call-process
		     program infile buffer display args))
      ad-do-it)))


;; compile.el parses the output for file names.  It expects them on
;; the local machine.  This must be changed.
;; `file-remote-p' cannot be used (yet) because it's a magic file name
;; function with Emacs 22 only.

(add-hook 'compilation-mode-hook
	  '(lambda ()
	     (set (make-local-variable 'comint-file-name-prefix)
		  (or (tramp-handle-file-remote-p default-directory) ""))))


;; gud.el uses `gud-find-file' for specifying a file name function.
;; Internally, it uses `gud-file-name' which we should do as well.
;; `gud-<MINOR-MODE>-directories' must be Tramp file names.

(defun tramp-gud-file-name (filename)
  (funcall
   'gud-file-name
   ;; Relative file names are expanded.
   (if (or (not (file-name-absolute-p filename) )
	   (tramp-handle-file-remote-p filename))
       filename
     (concat
      (tramp-handle-file-remote-p default-directory)
      filename))))

(add-hook 'gud-mode-hook
	  '(lambda ()
	     (set 'gud-find-file 'tramp-gud-file-name)))

(defadvice gud-perldb-massage-args
  (before tramp-advice-gud-perldb-massage-args (file args) activate)
  "Set arguments of Perl debugger on remote hosts.  They must be
changed to be relative to the default directory.  Works only for
relative file names and Tramp file names."
  (if (and default-directory (tramp-tramp-file-p default-directory))
      (let ((default-directory (expand-file-name default-directory))
	    (item args)
	    file)
	(while item
	  ;; The expansion is performed for EVERY parameter, even for
	  ;; non file names.  But this doesn't hurt, because it is
	  ;; changed back to its original value afterwards.
	  (setq file (expand-file-name (car item)))
	  (when (string-lessp default-directory file)
	    (setcar item (substring file (length default-directory))))
	  (setq item (cdr item))))))

(provide 'tramp-util)

;;; arch-tag: 500f9992-a44e-46d0-83a7-980799251808
;;; tramp-util.el ends here
