;;; tramp2-ops.el --- File operation handlers for TRAMP2

;; Copyright (C) 2001 by Daniel Pittman <daniel@rimspace.net>

;;; Commentary:

;; This defines the file operation handlers for TRAMP2

;; This is an example:

;; (def-tramp-handler sample-operation (file other-file)
;;   "This implements `sample-operation' for TRAMP2 files.

;; The argument FILE is magical - if this is a string, it is parsed to
;; a valid TRAMP2 path object. If it is already a path object, it is
;; left untouched.

;; This magic is based on the argument name. If you need to parse a
;; second file-name, you must do it by hand, as this example shows.

;; The handler is run from the buffer of the connection, so any output
;; will be available in the current buffer.

;; Use `tramp2-run-command' to execute a command on a remote machine
;; suitable for a path."
;;   (condition-case error
;;       (let ((other-file (tramp2-path-parse other-file)))
;;	 ;; Do things with the file here
;; 	) 
;;     (tramp2-file-error (t)))) ; not a valid tramp2 path





;;; Code:



(provide 'tramp2-ops)

;;; tramp2-ops.el ends here
