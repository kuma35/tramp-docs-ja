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

;; An implementation of file information caching for remote files.

;; Each connection (method, user, host) has a unique cache.
;; Each file has a unique set of properties associated with it.
;; Although method doesn't seem to be necessary, it allows a simpler
;; implementation.  And it doesn't matter, because it is rather rare a
;; remote host will be accessed using different methods in parallel.

;;; Code:

;; Maybe the code could be rearranged to avoid completely (method, user, host).
;; This would require that all functions are called from the connection buffer
;; only; something which could be forgotten.  And I have no idea whether
;; this is really a performance boost.

;; Another performance speedup could be the use of 'eq instead of 'equal for
;; the hash test function.  But this case the key, being a string, would need
;; to be transformed into a unique symbol, which would eat all saved time.

;; Pacify byte-compiler.
(autoload 'tramp-get-buffer "tramp")
(autoload 'tramp-message "tramp")
(autoload 'tramp-tramp-file-p "tramp")
(autoload 'with-parsed-tramp-file-name "tramp")

;;; -- Cache --

(defvar tramp-file-properties (make-hash-table :test 'equal)
  "Hash table for remote files properties.")

(defvar tramp-connection-properties (make-hash-table :test 'equal)
  "Hash table for persistent connection properties.")

(defcustom tramp-persistency-file-name "~/.tramp.eld"
  "File which keeps connection history for Tramp connections."
  :group 'tramp
  :type 'file)

(defun tramp-cache-setup (method user host)
  "Initialise the cache system for a new Tramp connection."
  (clrhash tramp-file-properties))

(defun tramp-get-file-property (method user host file property default)
  "Get the PROPERTY of FILE from the cache context of the
user USER on the remote machine HOST.  Return DEFAULT if not set."
  (let* ((file (directory-file-name file))
	 (key (make-tramp-file-name
	       :method method
	       :user user
	       :host host
	       :localname nil))
	 (hash1 (or (gethash key tramp-file-properties)
		    (puthash key (make-hash-table :test 'equal)
			     tramp-file-properties)))
	 (hash2 (or (gethash file hash1)
		    (puthash file (make-hash-table :test 'equal)
			     hash1)))
	 (value (if (hash-table-p hash2)
		    (gethash property hash2 default)
		  default)))
    (tramp-message 8 "%s %s %s" file property value)
    value))

(defun tramp-set-file-property (method user host file property value)
  "Set the PROPERTY of FILE to VALUE, in the cache context of the
user USER on the remote machine HOST.  Returns VALUE."
  (let* ((file (directory-file-name file))
	 (key (make-tramp-file-name
	       :method method
	       :user user
	       :host host
	       :localname nil))
	 (hash1 (or (gethash key tramp-file-properties)
		    (puthash key (make-hash-table :test 'equal)
			     tramp-file-properties)))
	 (hash2 (or (gethash file hash1)
		    (puthash file (make-hash-table :test 'equal)
			     hash1))))
    (puthash property value hash2)
    (tramp-message 7 "%s %s %s" file property value)
    value))

(defun tramp-flush-file-property (method user host file)
  "Remove all properties of FILE in the cache context of USER on HOST."
  (let* ((file (directory-file-name file))
	 (key (make-tramp-file-name
	       :method method
	       :user user
	       :host host
	       :localname nil))
	 (hash (or (gethash key tramp-file-properties)
		   (puthash key (make-hash-table :test 'equal)
			    tramp-file-properties))))
;    (tramp-message 7 "%s" (tramp-cache-print tramp-file-properties))
    (remhash file hash)))

(defun tramp-flush-directory-property (method user host directory)
  "Remove all properties of DIRECTORY in the cache context of USER on HOST.
Remove also properties of all files in subdirectories"
  (let* ((directory (directory-file-name directory))
	 (key (make-tramp-file-name
	       :method method
	       :user user
	       :host host
	       :localname nil))
	 (hash (or (gethash key tramp-file-properties)
		   (puthash key (make-hash-table :test 'equal)
			    tramp-file-properties))))
;    (tramp-message 7 "%s" (tramp-cache-print tramp-file-properties))
    (maphash
     '(lambda (key value)
	(when (string-match directory key) (remhash key hash)))
     hash)))

(defmacro with-file-property (method user host file property &rest body)
  "Check in Tramp cache for PROPERTY, otherwise execute BODY and set cache.
The cache will be set for absolute FILE names only; otherwise it is
not unique."
  `(if (file-name-absolute-p ,file)
       (let ((value (tramp-get-file-property
		     ,method ,user ,host ,file ,property 'undef)))
	 (when (eq value 'undef)
	   ;; We cannot pass ,@body as parameter to
	   ;; `tramp-set-file-property' because it mangles our
	   ;; debug messages.
	   (setq value (progn ,@body))
	   (tramp-set-file-property
	    ,method ,user ,host ,file ,property value))
	 value)
     ,@body))

(put 'with-file-property 'lisp-indent-function 5)
(put 'with-file-property 'edebug-form-spec t)

(defun tramp-cache-print (table)
  "Prints hash table TABLE."
  (when (hash-table-p table)
    (let (result tmp)
      (maphash
       '(lambda (key value)
	  (setq tmp (format
		     "(%s %s)"
		     (prin1-to-string key)
		     (if (hash-table-p value)
			 (tramp-cache-print value)
		       (prin1-to-string value)))
		result (if result (concat result " " tmp) tmp)))
       table)
      result)))

;; Reverting a buffer should also flush file properties.  They could
;; have been changed outside Tramp.

(defun tramp-cache-before-revert-function ()
  "Flush all Tramp cache properties from buffer-file-name."
  (let ((bfn (buffer-file-name))
	;; Pacify byte-compiler.
	method user host localname)
    (when (and (stringp bfn)
	       (tramp-tramp-file-p bfn))
      (with-parsed-tramp-file-name bfn nil
	(tramp-flush-file-property method user host localname)))))

(add-hook 'before-revert-hook 'tramp-cache-before-revert-function)
(add-hook 'tramp-cache-unload-hook
	  '(lambda ()
	     (remove-hook 'before-revert-hook
			  'tramp-cache-before-revert-function)))

;;; -- Properties --

(defun tramp-get-connection-property (method user host property default)
  "Get the named property for the connection.
If the value is not set for the connection, return DEFAULT."
  (let* ((key (make-tramp-file-name
	       :method method
	       :user user
	       :host host
	       :localname nil))
	 (hash (gethash key tramp-connection-properties))
	 (value (if (hash-table-p hash)
		   (gethash property hash default)
		 default)))
    (tramp-message 8 "%s %s" property value)
    value))

(defun tramp-set-connection-property (method user host property value)
  "Set the named property of a connection to VALUE.
The parameter TRANSIENT, if not nil, suppresses saving it in
persistent Tramp connection history."
  (let* ((key (make-tramp-file-name
	       :method method
	       :user user
	       :host host
	       :localname nil))
	 (hash (or (gethash key tramp-connection-properties)
		   (puthash key (make-hash-table :test 'equal)
			    tramp-connection-properties))))
    (puthash property value hash)
    (tramp-message 7 "%s %s" property value)
    value))

(defmacro with-connection-property (method user host property &rest body)
  "Check in Tramp for property PROPERTY, otherwise execute BODY and set.
PROPERTY is set persistent."
  (save-excursion
    `(let ((value
	    (tramp-get-connection-property
	     ,method ,user ,host ,property 'undef)))
       (when (eq value 'undef)
	 ;; We cannot pass ,@body as parameter to
	 ;; `tramp-set-connection-property' because it mangles our debug
	 ;; messages.
	 (setq value (progn ,@body))
	 (tramp-set-connection-property ,method ,user ,host ,property value))
       value)))

(put 'with-connection-property 'lisp-indent-function 4)
(put 'with-connection-property 'edebug-form-spec t)

(defun tramp-dump-connection-properties ()
  (when (and (hash-table-p tramp-connection-properties)
	     (not (zerop (hash-table-count tramp-connection-properties)))
	     (stringp tramp-persistency-file-name))
    ;; Dump it.
    (with-temp-buffer
      (insert
       ";; -*- emacs-lisp -*-"
       (condition-case nil
	   (progn
	     (require 'time-stamp)
	     (format
	      " <%s %s>\n"
	      (time-stamp-string "%02y/%02m/%02d %02H:%02M:%02S")
	      tramp-persistency-file-name))
	 (error "\n"))
       ";; Tramp connection history.  Don't change this file.\n"
       ";; You can delete it, forcing Tramp to reapply the checks.\n"
       (with-output-to-string
	 (pp
	  (read
	   (format "(%s)" (tramp-cache-print tramp-connection-properties))))))
      (write-region (point-min) (point-max) tramp-persistency-file-name))))

(add-hook 'kill-emacs-hook 'tramp-dump-connection-properties)
(add-hook 'tramp-cache-unload-hook
	  '(lambda ()
	     (remove-hook 'kill-emacs-hook
			  'tramp-dump-connection-properties)))

;; Read persistent connection history.
(eval-after-load "tramp"
  '(condition-case nil
       (when (file-exists-p tramp-persistency-file-name)
	 (with-temp-buffer
	   (insert-file-contents tramp-persistency-file-name)
	   (let ((list (read (current-buffer)))
		 element key item)
	     (while (setq element (pop list))
	       (setq key (pop element))
	       (while (setq item (pop element))
		 (tramp-set-connection-property
		  (elt key 1) (elt key 2) (elt key 3)
		  (pop item) (car item)))))))
     (error (clrhash tramp-connection-properties))))

(provide 'tramp-cache)

;;; tramp-cache.el ends here
