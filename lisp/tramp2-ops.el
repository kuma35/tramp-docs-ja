;;; tramp2-ops.el --- File operation handlers for TRAMP2

;; Copyright (C) 2001 Free Software Foundation, Inc.

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

(def-tramp-handler noop (file)
  "A simple remote command handler that changes nothing in the
remote system state."
  (tramp2-run-command file "true"))


(def-tramp-handler write-region (start end file &optional append visit lockname silly-emacs)
  "Write a region of the current buffer to a tramp2 file.
This behaves like `write-region' for tramp2 files.

This routine operates correctly, without surprises, on both
XEmacs and Emacs, with regard the 7th (SILLY-EMACS) argument.

LOCKNAME (and file locking) are ignored by this routine.

Version specific information:

In Emacs, argument 7 SILLY-EMACS is CONFIRM.
In XEmacs, argument 7 SILLY-EMACS is CODING-SYSTEM.

Specifically, from documentation for `write-region':

Write current region into specified file.
By default the file's existing contents are replaced by the specified region.
When called from a program, takes three arguments:
START, END and FILENAME.  START and END are buffer positions.
Optional fourth argument APPEND if non-nil means
  append to existing file contents (if any).
Optional fifth argument VISIT if t means
  set last-save-file-modtime of buffer to this file's modtime
  and mark buffer not modified.
If VISIT is a string, it is a second file name;
  the output goes to FILENAME, but the buffer is marked as visiting VISIT.
  VISIT is also the file name to lock and unlock for clash detection.
If VISIT is neither t nor nil nor a string,
  that means do not print the \"Wrote file\" message.
The optional sixth arg LOCKNAME, if non-nil, specifies the name to 
  use for locking and unlocking, overriding FILENAME and VISIT. 
Kludgy feature: if START is a string, then that string is written 
to the file, instead of any buffer contents, and END is ignored. 

XEmacs specific documentation:

Optional seventh argument CODING-SYSTEM specifies the coding system 
  used to encode the text when it is written out, and defaults to 
  the value of `buffer-file-coding-system' in the current buffer. 
  Interactively, with a prefix arg, you will be prompted for the 
  coding system.

Emacs specific documentation:

The optional seventh arg CONFIRM, if non-nil, says ask for confirmation
  before overwriting an existing file."
  ;; Handle the basic argument lossage. Dammit.
  (when (or (featurep 'xemacs)
	    (and (not (null silly-emacs))
		 (file-exists-p file)
		 (yes-or-no-p (format "File %s on host %s exists, overwrite? "
				      (tramp2-path-remote-file file)
				      (tramp2-connect-host (last (tramp2-path-connect file)))))))
    (tramp2-write-region-internal start end file
				  append visit lockname
				  (and (featurep 'xemacs)
				       silly-emacs))))

(defun tramp2-write-region-internal (start end file append visit lockname coding-system)
  "Internal handler for `write-region' once the Emacs/XEmacs lossage
is dealt with.

Locking, and LOCKNAME specifically, are ignored."
  ;; Handle VISIT here, which is non-trivial, I think. :/
  ;; Handle writing. The sane simple case, well, is. I wonder if we /ever/
  ;; hit that code path... --daniel
  (if (and (not (stringp start))
	   (null coding-system))
      (tramp2-write start end file append)
    ;; Nope. Do the setup to make it usable
    (let ((buffer (tramp2-write-region-setup start end file append
					     visit lockname coding-system))
	  result)
      (unwind-protect
	  (with-current-buffer buffer
	    (setq result (tramp2-write (point-min) (point-max) file append)))
	(when (buffer-live-p buffer)
	  (kill-buffer buffer)))
      ;; Make sure the result makes it out...
      result)))
  

(defun tramp2-write-region-setup (start end file append visit lockname coding-system)
  "Create a buffer that contains the data for sending to the remote system.
This buffer is destroyed after use and should contain a copy of the original
data.

Note that CODING-SYSTEM should be nil on Emacs; the code is untested and
almost certainly wrong under it's MULE implementation."
  (let ((buffer (generate-new-buffer " *tramp2 data send buffer*"))
	(source (current-buffer)))
    ;; Insert the source data...
    (with-current-buffer buffer
      (if (stringp start)
	  (insert start)
	(insert-buffer-substring source start end)))

    ;; Do the coding-system massaging, if desired.
    (when coding-system
      (let ((filename (tramp2-path-construct file)))
	(setq coding-system
	      (or coding-system-for-write
		  (run-hook-with-args-until-success
		   'write-region-pre-hook start end filename append visit lockname)
		  coding-system
		  buffer-file-coding-system
		  (find-file-coding-system-for-write-from-filename filename)))
	;; If this is a cons, the system code gives up. I have no better
	;; ideas, I admit. Try to cope with the error, though, just in
	;; case it's the most common case and we corrupt data...
	(when (consp coding-system)
	  (signal-error 'tramp2-file-error
			"Coding system is a cons, can't cope!" coding-system))
	;; Now the actual code that does the right thing by the system code...
	(unless (consp coding-system)
	  (let ((func (coding-system-property coding-system 'pre-write-conversion)))
	    (when func
	      (let ((modif (buffer-modified-p)))
		(with-current-buffer buffer
		  (funcall func (point-min) (point-max)))))))))

    ;; Return the buffer to the caller...
    buffer))

    
    
	



(provide 'tramp2-ops)

;; TODO:
;; * `write-region' needs *far* better support for MULE/coding-system
;;   under XEmacs. See the defn on a MULE-capable XEmacs.


;;; tramp2-ops.el ends here
