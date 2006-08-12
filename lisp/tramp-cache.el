;;; -*- mode: Emacs-Lisp; coding: iso-2022-7bit; byte-compile-dynamic: t; -*-
;;; tramp-cache.el --- file information caching for Tramp

;; Copyright (C) 2000, 2005, 2006 by Free Software Foundation, Inc.

;; Author: Daniel Pittman <daniel@inanna.danann.net>
;;         Michael Albinus <michael.albinus@gmx.de>
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

;; An implementation of information caching for remote files.

;; Each connection, identified by a vector [method user host
;; localname] or by a process, has a unique cache. We distinguish 3
;; kind of caches, depending on the key:
;;
;; - localname is NIL.  This are reusable properties.  Examples:
;;   "remote-shell" identifies the POSIX shell to be called on the
;;   remote host, or "perl" is the command to be called on the remote
;;   host, when starting a Perl script.  These properties are saved in
;;   the file `tramp-persistency-file-name'.
;;
;; - localname is a string.  This are temporary properties, which are
;;   related to the file localname is referring to.  Examples:
;;   "file-exists-p" is t or nile, depending on the file existence, or
;;   "file-attributes" caches the result of the function
;;  `file-attributes'.
;;
;; - The key is a process.  This are temporary properties related to
;;   an open connection.  Examples: "scripts" keeps to shell script
;;   definitions already sent to the remote shell, "last-cmd-time" is
;;   the time stamp a command has been sent to the remote process.

;;; Code:

;; Pacify byte-compiler.
(eval-when-compile
  (autoload 'tramp-message "tramp")
  (autoload 'tramp-tramp-file-p "tramp")
  (autoload 'tramp-file-name-method "tramp")
  (autoload 'tramp-file-name-user "tramp")
  (autoload 'tramp-file-name-host "tramp")
  (autoload 'tramp-file-name-localname "tramp")
  (autoload 'with-parsed-tramp-file-name "tramp")
  (autoload 'time-stamp-string "time-stamp"))

;;; -- Cache --

(defvar tramp-cache-data (make-hash-table :test 'equal)
  "Hash table for remote files properties.")

(defcustom tramp-persistency-file-name
  (cond
   ((and (not (featurep 'xemacs)) (file-directory-p "~/.emacs.d/"))
    "~/.emacs.d/tramp")
   ((and (featurep 'xemacs) (file-directory-p "~/.xemacs/"))
    "~/.xemacs/tramp")
   ;; For users without `~/.emacs.d/' or `~/.xemacs/'.
   (t "~/.tramp"))
  "File which keeps connection history for Tramp connections."
  :group 'tramp
  :type 'file)

(defun tramp-get-file-property (vec file property default)
  "Get the PROPERTY of FILE from the cache context of VEC.
Returns DEFAULT if not set."
  ;; Unify localname.
  (aset vec 4 (directory-file-name file))
  (let* ((hash (or (gethash vec tramp-cache-data)
		   (puthash vec (make-hash-table :test 'equal)
			    tramp-cache-data)))
	 (value (if (hash-table-p hash)
		    (gethash property hash default)
		  default)))
    (tramp-message vec 8 "%s %s %s" file property value)
    value))

(defun tramp-set-file-property (vec file property value)
  "Set the PROPERTY of FILE to VALUE, in the cache context of VEC.
Returns VALUE."
  ;; Unify localname.
  (aset vec 4 (directory-file-name file))
  (let ((hash (or (gethash vec tramp-cache-data)
		  (puthash vec (make-hash-table :test 'equal)
			   tramp-cache-data))))
    (puthash property value hash)
    (tramp-message vec 8 "%s %s %s" file property value)
    value))

(defun tramp-flush-file-property (vec file)
  "Remove all properties of FILE in the cache context of VEC."
  ;; Unify localname.
  (aset vec 4 (directory-file-name file))
  (tramp-message vec 8 "%s" file)
  (remhash vec tramp-cache-data))

(defun tramp-flush-directory-property (vec directory)
  "Remove all properties of DIRECTORY in the cache context of VEC.
Remove also properties of all files in subdirectories."
  (let ((directory (directory-file-name directory)))
  (tramp-message vec 8 "%s" directory)
    (maphash
     '(lambda (key value)
	(when (string-match directory (tramp-file-name-localname key))
	  (remhash key tramp-cache-data)))
     tramp-cache-data)))

(defmacro with-file-property (vec file property &rest body)
  "Check in Tramp cache for PROPERTY, otherwise execute BODY and set cache.
FILE must be a local file name on a connection identified via VEC."
  `(if (file-name-absolute-p ,file)
      (let ((value (tramp-get-file-property ,vec ,file ,property 'undef)))
	(when (eq value 'undef)
	  ;; We cannot pass @body as parameter to
	  ;; `tramp-set-file-property' because it mangles our
	  ;; debug messages.
	  (setq value (progn ,@body))
	  (tramp-set-file-property ,vec ,file ,property value))
	value)
     ,@body))

(put 'with-file-property 'lisp-indent-function 3)
(put 'with-file-property 'edebug-form-spec t)

(defun tramp-cache-print (table)
  "Prints hash table TABLE."
  (when (hash-table-p table)
    (let (result tmp)
      (maphash
       '(lambda (key value)
	  (setq tmp (format
		     "(%s %s)"
		     (if (processp key)
			 (prin1-to-string (prin1-to-string key))
		       (prin1-to-string key))
		     (if (hash-table-p value)
			 (tramp-cache-print value)
		       (if (bufferp value)
			   (prin1-to-string (prin1-to-string value))
			 (prin1-to-string value))))
		result (if result (concat result " " tmp) tmp)))
       table)
      result)))

;; Reverting a buffer should also flush file properties.  They could
;; have been changed outside Tramp.
(defun tramp-cache-before-revert-function ()
  "Flush all Tramp cache properties from buffer-file-name."
  (let ((bfn (buffer-file-name))
	;; Pacify byte-compiler.
	v localname)
    (when (and (stringp bfn) (tramp-tramp-file-p bfn))
      (with-parsed-tramp-file-name bfn nil
	(tramp-flush-file-property v localname)))))

(add-hook 'before-revert-hook 'tramp-cache-before-revert-function)
(add-hook 'tramp-cache-unload-hook
	  '(lambda ()
	     (remove-hook 'before-revert-hook
			  'tramp-cache-before-revert-function)))

;;; -- Properties --

(defun tramp-get-connection-property (key property default)
  "Get the named PROPERTY for the connection.
KEY identifies the connection, it is either a process or a vector.
If the value is not set for the connection, returns DEFAULT."
  ;; Unify key by removing localname from vector.
  (when (vectorp key) (aset key 4 nil))
  (let* ((hash (gethash key tramp-cache-data))
	 (value (if (hash-table-p hash)
		   (gethash property hash default)
		 default)))
    (tramp-message key 7 "%s %s" property value)
    value))

(defun tramp-set-connection-property (key property value)
  "Set the named PROPERTY of a connection to VALUE.
KEY identifies the connection, it is either a process or a vector.
PROPERTY is set persistent when KEY is a vector."
  ;; Unify key by removing localname from vector.
  (when (vectorp key) (aset key 4 nil))
  (let ((hash (or (gethash key tramp-cache-data)
		  (puthash key (make-hash-table :test 'equal)
			    tramp-cache-data))))
    (puthash property value hash)
    ;; This function is called also during initialization of
    ;; tramp-cache.el.  `tramp-message´ is not defined yet at this
    ;; time, so we ignore the corresponding error.
    (condition-case nil
	(tramp-message key 7 "%s %s" property value)
      (error nil))
    value))

(defun tramp-flush-connection-property (key event)
  "Remove all properties identified by KEY.
KEY identifies the connection, it is either a process or a
vector.  EVENT is not used, it is just applied because this
function is intended to run also as process sentinel."
  ;; Unify key by removing localname from vector.
  (when (vectorp key) (aset key 4 nil))
;  (tramp-message key 7 "%s" event)
  (remhash key tramp-cache-data))

(defmacro with-connection-property (key property &rest body)
  "Checks in Tramp for property PROPERTY, otherwise executes BODY and set."
  `(let ((value (tramp-get-connection-property ,key ,property 'undef)))
    (when (eq value 'undef)
      ;; We cannot pass ,@body as parameter to
      ;; `tramp-set-connection-property' because it mangles our debug
      ;; messages.
      (setq value (progn ,@body))
      (tramp-set-connection-property ,key ,property value))
    value))

(put 'with-connection-property 'lisp-indent-function 2)
(put 'with-connection-property 'edebug-form-spec t)

(defun tramp-dump-connection-properties ()
"Writes persistent connection properties into file
`tramp-persistency-file-name'."
  ;; We shouldn't fail, otherwise (X)Emacs might not be able to be closed.
  (condition-case nil
      (when (and (hash-table-p tramp-cache-data)
		 (not (zerop (hash-table-count tramp-cache-data)))
		 (stringp tramp-persistency-file-name))
	(let ((cache (copy-hash-table tramp-cache-data)))
	  ;; Remove temporary data.
	  (maphash
	   '(lambda (key value)
	      (if (and (vectorp key) (not (tramp-file-name-localname key)))
		  (progn
		    (remhash "process-name" value)
		    (remhash "process-buffer" value))
		(remhash key cache)))
	   cache)
	  ;; Dump it.
	  (with-temp-buffer
	    (insert
	     ";; -*- emacs-lisp -*-"
	     ;; `time-stamp-string' might not exist in all (X)Emacs flavors.
	     (condition-case nil
		 (progn
		   (format
		    " <%s %s>\n"
		    (time-stamp-string "%02y/%02m/%02d %02H:%02M:%02S")
		    tramp-persistency-file-name))
	       (error "\n"))
	     ";; Tramp connection history.  Don't change this file.\n"
	     ";; You can delete it, forcing Tramp to reapply the checks.\n\n"
	     (with-output-to-string
	       (pp (read (format "(%s)" (tramp-cache-print cache))))))
	    (write-region
	     (point-min) (point-max) tramp-persistency-file-name))))
    (error nil)))

(add-hook 'kill-emacs-hook 'tramp-dump-connection-properties)
(add-hook 'tramp-cache-unload-hook
	  '(lambda ()
	     (remove-hook 'kill-emacs-hook
			  'tramp-dump-connection-properties)))

(defun tramp-parse-connection-properties (method)
  "Return a list of (user host) tuples allowed to access for METHOD.
This function is added always in `tramp-get-completion-function'
for all methods.  Resulting data are derived from connection
history."
  (let (res)
    (maphash
     '(lambda (key value)
	(if (and (vectorp key)
		 (string-equal method (tramp-file-name-method key))
		 (not (tramp-file-name-localname key)))
	    (push (list (tramp-file-name-user key)
			(tramp-file-name-host key))
		  res)))
     tramp-cache-data)
    res))

;; Read persistent connection history.  Applied with
;; `load-in-progress', because it shall be evaluated only once.
(when load-in-progress
  (condition-case err
      (with-temp-buffer
	(insert-file-contents tramp-persistency-file-name)
	(let ((list (read (current-buffer)))
	      element key item)
	  (while (setq element (pop list))
	    (setq key (pop element))
	    (while (setq item (pop element))
	      (tramp-set-connection-property key (pop item) (car item))))))
    (file-error
     ;; Most likely because the file doesn't exist yet.  No message.
     (clrhash tramp-cache-data))
    (error
     ;; File is corrupted.
     (message "%s" (error-message-string err))
     (clrhash tramp-cache-data))))

(provide 'tramp-cache)

;;; tramp-cache.el ends here
