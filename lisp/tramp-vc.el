;;; tramp-vc.el --- Version control integration for TRAMP.el

;; Copyright (C) 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Daniel Pittman <daniel@danann.net>
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; See the main module, 'tramp.el' for discussion of the purpose of TRAMP.
;; This module provides integration between remote files accessed by TRAMP and
;; the Emacs version control system.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'vc)
;; Old VC defines vc-rcs-release in vc.el, new VC requires extra module.
(unless (boundp 'vc-rcs-release)
  (require 'vc-rcs))
(require 'tramp)

;; Avoid byte-compiler warnings if the byte-compiler supports this.
;; Currently, XEmacs supports this.
(eval-when-compile
  (when (featurep 'xemacs)
    (byte-compiler-options (warnings (- unused-vars)))))

;; -- vc --

;; This used to blow away the file-name-handler-alist and reinstall
;; TRAMP into it. This was intended to let VC work remotely. It didn't,
;; at least not in my XEmacs 21.2 install.
;;
;; In any case, tramp-run-real-handler now deals correctly with disabling
;; the things that should be, making this a no-op.
;;
;; I have removed it from the tramp-file-name-handler-alist because the
;; shortened version does nothing. This is for reference only now.
;;
;; Daniel Pittman <daniel@danann.net>
;;
;; (defun tramp-handle-vc-registered (file)
;;   "Like `vc-registered' for tramp files."
;;   (tramp-run-real-handler 'vc-registered (list file)))



;; Do we need to advise the vc-user-login-name function anyway?
;; This will return the correct login name for the owner of a
;; file. It does not deal with the default remote user name...
;;
;; That is, when vc calls (vc-user-login-name), we return the
;; local login name, something that may be different to the remote
;; default.
;;
;; The remote VC operations will occur as the user that we logged
;; in with however - not always the same as the local user.
;;
;; In the end, I did advise the function. This is because, well,
;; the thing didn't work right otherwise ;)
;;
;; Daniel Pittman <daniel@danann.net>

(defun tramp-handle-vc-user-login-name (&optional uid)
  "Return the default user name on the remote machine.
Whenever VC calls this function, `file' is bound to the file name
in question.  If no uid is provided or the uid is equal to the uid
owning the file, then we return the user name given in the file name.

This should only be called when `file' is bound to the
filename we are thinking about..."
  ;; Pacify byte-compiler; this symbol is bound in the calling
  ;; function.  CCC: Maybe it would be better to move the
  ;; boundness-checking into this function?
  (let* ((file (symbol-value 'file))
	 (remote-uid
	  ;; With Emacs 22, `file-attributes' has got an optional parameter
	  ;; ID-FORMAT. Handle this case backwards compatible.
	  (if (and (functionp 'subr-arity)
		   (= 2 (cdr (funcall (symbol-function 'subr-arity)
				      (symbol-function 'file-attributes)))))
	      (nth 2 (file-attributes file 'integer))
	    (nth 2 (file-attributes file)))))
    (if (and uid (/= uid remote-uid))
	(error "tramp-handle-vc-user-login-name cannot map a uid to a name")
      (let* ((v (tramp-dissect-file-name (expand-file-name file)))
	     (u (tramp-file-name-user v)))
	(cond ((stringp u) u)
	      ((vectorp u) (elt u (1- (length u))))
	      ((null    u) (user-login-name))
	      (t	   (error "tramp-handle-vc-user-login-name cannot cope!")))))))


(defadvice vc-user-login-name
  (around tramp-vc-user-login-name activate)
  "Support for files on remote machines accessed by TRAMP."
  ;; We rely on the fact that `file' is bound when this is called.
  ;; This appears to be the case everywhere in vc.el and vc-hooks.el
  ;; as of Emacs 20.5.
  ;;
  ;; CCC TODO there should be a real solution!  Talk to Andre Spiegel
  ;; about this.
  (let ((file (when (boundp 'file)
                (symbol-value 'file))))    ;pacify byte-compiler
    (or (and (stringp file)
             (tramp-tramp-file-p file)	; tramp file
             (setq ad-return-value
		   (save-match-data
		     (tramp-handle-vc-user-login-name uid)))) ; get the owner name
        ad-do-it)))                     ; else call the original


;; Wire ourselves into the VC infrastructure...
;; This function does not exist any more in Emacs-21's VC
;; CCC: it appears that no substitute is needed for Emacs 21.
(defadvice vc-file-owner
  (around tramp-vc-file-owner activate)
  "Support for files on remote machines accessed by TRAMP."
  (let ((filename (ad-get-arg 0)))
    (or (and (tramp-tramp-file-p filename)
             (setq ad-return-value
		   (nth 2 (tramp-handle-file-attributes filename 'string))))
	ad-do-it)))


;; We need to make the version control software backend version
;; information local to the current buffer. This is because each TRAMP
;; buffer can (theoretically) have a different VC version and I am
;; *way* too lazy to try and push the correct value into each new
;; buffer.
;;
;; Remote VC costs will just have to be paid, at least for the moment.
;; Well, at least, they will right until I feel guilty about doing a
;; botch job here and fix it. :/
;;
;; Daniel Pittman <daniel@danann.net>
;; CCC: this is probably still needed for Emacs 21.
(defun tramp-vc-setup-for-remote ()
  "Make the backend release variables buffer local.
This makes remote VC work correctly at the cost of some processing time."
  (when (and (buffer-file-name)
             (tramp-tramp-file-p (buffer-file-name)))
    (make-local-variable 'vc-rcs-release)
    (setq vc-rcs-release nil)))
(add-hook 'find-file-hooks 'tramp-vc-setup-for-remote t)

;; No need to load this again if anyone asks.
(provide 'tramp-vc)

;;; arch-tag: 27cc42ce-da19-468d-ad5c-a2690558db60
;;; tramp-vc.el ends here
