;;; tramp2-util.el --- TRAMP2 utility functions

;; Copyright (C) 2001 by Daniel Pittman <daniel@localhost>

;;; Commentary:

;; This module contains routines that are likely to be valuable to
;; user-defined tramp2 code or that are used in multiple modules and don't
;; really fit into the main tramp2.el file.

;;; Code:

(defun tramp2-util-shell-write (file coder append data)
  "Write DATA to FILE on the remote machine, appending to it if
APPEND is not nil. DATA is the encoded data to transmit to the
remote machine and CODER is the executable to decode data on the
remote machine.

This routine returns t if the call succeeds and nil otherwise."
  (let ((end-of-data (format (tramp2-make-bracket-string) "end-of-data" "")))
    (= 0 (tramp2-run-command file (format "%s <<'%s' %s %s\n%s\n%s\n"
					  coder
					  end-of-data
					  ;; Destination...
					  (if append ">>" ">")
					  (tramp2-path-remote-file file)
					  ;; Encdoded data
					  data
					  ;; End-of-data marker.
					  end-of-data)))))


(defun tramp2-util-shell-read (file encoder decoder start end)
  "Read the bytes from START to END from file on the remote machine.
Use ENCODER to encode it on the remote machine and DECODER to
decode it locally.

ENCODER must be a string, a shell command to run.
DECODER must be a function that accepts two arguments, the start
and end point of the encoded data in the current buffer."
  (when (and start end (> start end))
    (signal-error 'tramp2-file-error
		  (format "End of data %s less than start of data %s" end start)))
  (unless (and (stringp encoder)
	       (fboundp decoder))
    (signal-error 'tramp2-file-error
		  (format "Invalid coder specified" encoder decoder)))
  ;; Right, do the actual hacking...
  (tramp2-with-connection file
    (let ((cut (when (and start end tramp2-dd)
		 (format "{ 2>/dev/null %s bs=1 skip=%s count=%s } |"
			 tramp2-dd start (- end start)))))
      (when (= 0 (tramp2-run-command file (format "<%s %s %s"
						  (tramp2-path-remote-file file)
						  (or cut "") encoder)))
	;; Got the data, yay.
	(funcall decoder (point-min) (point-max))
	;; Did we clip with dd?
	(when (and start end (not tramp2-dd))
	  ;; Nope, clip ourselves. Sadness.
	  (delete-region (point-min) start)
	  (delete-region end (point-max)))
	;; Return the buffer with the decoded data...
	(current-buffer)))))
				       

(defun tramp2-temp-file-name ()
  "Return a file-name suitable for a temporary file.
FILE is used as a basis for making it unique and all."
  (let* ((dir (file-name-as-directory (or (and (boundp 'tramp2-temp-dir) tramp2-temp-dir)
					  (getenv "TMPDIR")
					  (getenv "TMP")
					  "/tmp")))
	 (prefix (or (and (boundp 'tramp2-temp-name-prefix) tramp2-temp-name-prefix)
		     "tramp2."))
	 ;; Generate the temporary name...
	 (path (funcall (or (and (fboundp 'make-temp-name) #'make-temp-name)
			    (and (fboundp 'make-temp-file) #'make-temp-file)
			    #'tramp2-temp-file-name-internal)
			(concat dir prefix))))
    path))

(defun tramp2-temp-file-name-internal (prefix)
  "If no suitable temp-name generator exists on this system, fake it."
  (apply 'format "%s.%s.%s%s%s" prefix (emacs-pid) (current-time)))
	  



(provide 'tramp2-util)

;;; tramp2-util.el ends here
