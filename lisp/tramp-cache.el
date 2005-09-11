;;; -*- mode: Emacs-Lisp; coding: iso-2022-7bit; byte-compile-dynamic: t; -*-
;;; tramp-cache.el --- file information caching for Tramp

;; Copyright (C) 2000, 2005 by Free Software Foundation, Inc.

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

;; We wrap `tramp-get-buffer' and `tramp-message' with `with-no-warnings'
;; in order to have a cheap byte-compiler satisfaction.

;; Maybe the code could be rearranged to avoid completely (method, user, host).
;; This would require that all functions are called from the connection buffer
;; only; something which could be forgotten.  And I have no idea whether
;; this is really a performance boost.

;; Another performance speedup could be the use of 'eq instead of 'equal for
;; the hash test function.  But this case the key, being a string, would need
;; to be transformed into a unique symbol, which would eat all saved time.

;; Pacify byte-compiler.  Function `with-no-warnings' exists in Emacs 21 only.
(eval-and-compile
  (unless (functionp 'with-no-warnings) (fset 'with-no-warnings 'eval)))

(defvar tramp-cache-data nil
  "Hash table for remote files properties.
This variable is automatically made buffer-local to each process buffer
upon opening the connection.")

(defun tramp-cache-setup (method user host)
  "Initialise the cache system for a new Tramp connection."
  (with-current-buffer (with-no-warnings (tramp-get-buffer method user host))
    (set (make-local-variable 'tramp-cache-data)
	 (make-hash-table :test 'equal))))

(defun tramp-cache-get-file-property (method user host file key default)
  "Get the property KEY of FILE from the cache context of the
user USER on the remote machine HOST.  Return DEFAULT if not set."
  (with-current-buffer (with-no-warnings (tramp-get-buffer method user host))
    (unless (hash-table-p tramp-cache-data)
      (tramp-cache-setup method user host))
    (let* ((file (directory-file-name file))
	   (hash (gethash file tramp-cache-data))
	   (prop (if (hash-table-p hash) (gethash key hash default) default)))
      (with-no-warnings
	(tramp-message 10 "%s %s %s" file key prop))
      prop)))

(defun tramp-cache-set-file-property (method user host file key value)
  "Set the property KEY of FILE to VALUE, in the cache context of the
user USER on the remote machine HOST.  Returns VALUE."
  (with-current-buffer (with-no-warnings (tramp-get-buffer method user host))
    (unless (hash-table-p tramp-cache-data)
      (tramp-cache-setup method user host))
    (let ((file (directory-file-name file)))
      (puthash key value
	       (or (gethash file tramp-cache-data)
		   (puthash file (make-hash-table :test 'equal)
			    tramp-cache-data)))
      (with-no-warnings
	(tramp-message 10 "%s %s %s" file key value))
      value)))

(defun tramp-cache-flush-file (method user host file)
  "Remove all properties of FILE in the cache context of USER on HOST."
  (with-current-buffer (with-no-warnings (tramp-get-buffer method user host))
    (let* ((file (directory-file-name file)))
;      (with-no-warnings
;	(tramp-message 10 "%s" (tramp-cache-print tramp-cache-data)))
      (remhash file tramp-cache-data))))

(defun tramp-cache-flush (method user host)
  "Remove all information from the cache context of USER on HOST."
  (with-current-buffer (with-no-warnings (tramp-get-buffer method user host))
    (clrhash tramp-cache-data)))

(defmacro with-cache-data (method user host file key &rest body)
  "Check in Tramp cache for KEY, otherwise execute BODY and set cache.
The cache will be set for absolute FILE names only; otherwise it is
not unique."
  `(if (file-name-absolute-p ,file)
       (let ((value (tramp-cache-get-file-property
		     ,method ,user ,host ,file ,key 'undef)))
	 (when (eq value 'undef)
	   ;; We cannot pass ,@body as parameter to
	   ;; `tramp-cache-set-file-property' because it mangles our
	   ;; debug messages.
	   (setq value ,@body)
	   (tramp-cache-set-file-property
	    ,method ,user ,host ,file ,key value))
	 value)
     ,@body))

(put 'with-cache-data 'lisp-indent-function 5)

(defun tramp-cache-print (table)
  "Prints hash table TABLE."
  (let (result tmp)
    (maphash
     '(lambda (key value)
	(setq tmp (format "(%s %s)" key
			  (if (hash-table-p value)
			      (tramp-cache-print value)
			    value))
	      result (if result (concat result " " tmp) tmp)))
     table)
    result))

(provide 'tramp-cache)

;;; tramp-cache.el ends here
