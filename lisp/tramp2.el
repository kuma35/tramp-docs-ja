;;; tramp2.el --- Network file access via login shell

;; Copyright (C) 2001 Free Software Foundation, Inc.

;;; Commentary:

;; Based on the TRAMP code by Kai Groﬂjohann, et al.

;;; Code:

(require 'cl)				
(require 'timer)
(require 'shell)

(defconst tramp2-version "$Id: tramp2.el,v 2.6 2001/03/08 08:07:12 daniel Exp $"
  "The CVS version number of this tramp2 release.")


;; Error thrown when a file is invalid.
(define-error 'tramp2-file-error
  "Error thrown when a tramp2 specific error occurs.
Inheritance ensures that anything expecting generic file errors will be happy."
  'file-error)


(defvar tramp2-load-hooks nil
  "*Hooks run when the tramp2 file interface is loaded.")

(defconst tramp2-path-tag "/!:"
  "Regular expression matching a TRAMP2 path tag.")

;; Internal.
(defconst tramp2-path-connect (concat ;; match a protocol statement
			              ;; 1     2      2        3            3   1
				      "\\(\\[\\([^]]+\\)\\]\\|\\([a-zA-Z]+\\)|\\)?"
				      ;; match a user statement
				      ;; 4  5                5   4
				      "\\(\\([-_a-zA-Z0-9]+\\)@\\)?"
				      ;; match a host
				      ;; 6               6
				      "\\([-a-zA-Z0-9]+\\)?"
				      ":")
  "Regular expression matching a single complete connect expression.
This does not (and can't cleanly) represent all the rules for a /valid/
connect expression, but it simplifies the first-stage approximation well.")

(defvar tramp2-shell-prompt-pattern (list
				     (cons 'default shell-prompt-pattern))
  "A set of regular expressions to match the prompt of a remote host.
Values in this are looked up with `tramp2-find-value'.")


(defvar tramp2-remote-shell-alist (list
				   '(default ("/bin/sh -i"
					      "/bin/bash -i"
					      "/bin/ksh -i")))
  "Define the remote shell to run on a particular host.
Values in this are looked up with `tramp2-find-value' and the
result is treated as an active expression (`tramp2-expression').

The shell run by executing the command-line given should be an
interactive Bourne shell capable of expanding a '~' into the
home directory of a user.

If the value is a list of strings, these strings are tested in
order to detect which of them supports tilde expansion. The
default set should work with the auto-detection support on
most systems.")


;; REVISIT: Internal...
(defvar tramp2-handler-alist nil
  "Associative list of tramp2 file operation handlers.
To define a file operation handler, see the `def-tramp-handler' macro.

This list is automatically generated. You shouldn't change this
by hand.")


;; REVISIT: What should this be in the release?
(defvar tramp2-default-protocol 'ssh
  "The default protocol to use.")

;; REVISIT: Populate this with a larger number of connections.
(defvar tramp2-protocol-alist '((ssh . ((command . tramp2-ssh-make-command)))

				;; REVISIT: This is probably not worth the
				;; effort long term. Debug only.
				(shell . ((command . "sh -i"))))
  "An associative set of protocol tags, each mapping to an alist
defining the characteristics of the connection.

Each protocol has a symbol as a tag. The defined characteristics are:

* `command', the command line to execute. A tramp2 active expression.
  See `tramp2-expression' for more details.

* `encoding', the encoding to use for the connection.
  If this is unspecified, an inline transfer encoding is automatically
  detected on the remote machine. See `tramp2-encoding-alist' for
  more details.")

   
(defvar tramp2-ssh-executable (list
			       '(default "ssh"))
  "Arguments to provide to an ssh connection.
Values in this are looked up with `tramp2-find-value' and the
result treated with `tramp2-expression'.")


(defvar tramp2-ssh-arguments (list
			      '(default "-t -e none"))
  "Arguments to provide to an ssh connection.
Values in this are looked up with `tramp2-find-value' and the
result treated with `tramp2-expression'.")



(defvar tramp2-encoding-alist '((base64 . ((test  . tramp2-base64-test)
					   (write . tramp2-base64-write)
					   (read  . tramp2-base64-read)))
				
				(uuencode . ((test  . tramp2-uuencode-test)
					     (write . tramp2-uuencode-write)
					     (read  . tramp2-uuencode-read))))
  "An associative list of encoding types and their properties.
Each encoding has a name and a number of properties. Each property
is a symbol representing a function to call to achieve a specified
result.

* `test', a function to test if the encoding is suitable for use
  with a given connection.

  It is called with two arguments, the final connection and the
  path that triggered the connection. It should return `t' if the
  encoding is suitable and `nil' otherwise.

  If this property is not present, the encoding will be used if
  specified in a protocol without verification, and will not be
  detected automatically on a remote machine.

* `write', a function to send data to the remote machine.
  It is a function name that is called with five arguments,
  SOURCE, START, END, FILE and APPEND.

  SOURCE is the buffer that holds the data to be sent. This data
  *must not* be changed by this routine.

  START and END are positions in the SOURCE buffer. The data from
  START to END should be written to the remote file.

  FILE is the full tramp2 path of the file to write.

  If APPEND is non-nil, the data should be appended to the file,
  if it already exists. The file should be overwritten otherwise.

  This routine *must not* change the current buffer.


* `read', a function to retrieve a file from the remote machine.
  It will be called in the connection buffer for a connection and
  will be given the local and remote file names to operate on.")


(defvar tramp2-base64-coder
  (list
   `(default ((encoder ("mimencode"
			"recode ../64"
			,(concat "perl -e 'use MIME::Base64 qw(encode_base64);"
				  "$/ = undef; print encode_base64(<STDIN>);'")))
	      (decoder ("mimencode -u"
			"recode /64.."
			,(concat "perl -e 'use MIME::Base64 qw(decode_base64);"
				  "$/ = undef; print decode_base64(<STDIN>);'"))))))
  "An associative list of base64 coding programs for remote machines.
Values in this are looked up with `tramp2-find-value'.

The value is a list of properties with the following predefined:

* `encoder', the remote command to encode to base64.
* `decoder', the remote command to decode from base64.

Each of these may be a string, in which case they are used as a
command directly, or a list of strings in which case each command
is tried in turn until one is found that succeeds.

The encoder and decoder command need not be the same executable
or even the same item in the list.")



;; REVISIT: Semi-public, fill this in.
(defvar tramp2-connect-actors (list
			      '(tramp2-shell-prompt . (throw 'ready t)))
  "A list of actions to take while connecting to a remote machine.
See `tramp2-run-actors' for details on the content of this list.

This set of actions is run while establishing each hop in the connection
sequence. Matching for password prompts and similar questions should
go here.")


(defvar tramp2-shell-startup-actors (list
				     '(tramp2-shell-prompt . (throw 'ready t)))
  "A list of actions to take while executing a remote login shell.
See `tramp2-run-actors' for details on the content of this list.

This set of actions is run while executing a suitable login shell
on the remote machine.")



;; REVISIT: Semi-public.
(defvar tramp2-setup-functions '(tramp2-setup-interactive-shell
				 tramp2-setup-remote-environment
				 tramp2-setup-file-transfer)
  "The list of functions to run, in order, to setup the remote shell.
This is run in the tramp2 connection buffer and should run commands
to ensure that the remote shell is ready to accept commands.

The function is run in the connection buffer. Setup functions must
accept two arguments, the connect object for the final hop of the
connection and the full path that triggered the request.

See `tramp2-send-command' for details on sending a command to the
remote system.

Note that you almost certainly *DON'T* want to make any function
other than `tramp2-setup-interactive-shell' the first function in
this list.

If you do, you should be aware that `tramp2-send-command' (amongst
other things) will not work.")


(defconst tramp2-shell-default-environment '(("PATH"      tramp2-shell-path)
					     ("TERM"      "dumb")
					     ("HISTFILE"  nil)
					     ("MAIL"      nil)
					     ("MAILCHECK" nil)
					     ("MAILPATH"  nil)
					     ("CDPATH"    nil)
					     ("LC_TIME"   "C"))
  "Default remote environment values set into the remote shell.
The values here can be overridden by values in `tramp2-shell-environment'.

You should not need to change the values in here directly. Values
in this list are processed in the same way as values in the
`tramp2-shell-environment' list.")


(defvar tramp2-shell-environment nil
  "Remote environment values to set for the remote shell.
Values in this are looked up with `tramp2-find-value'.

The value is a list of values to set into the remote environment.
Each entry in the list is a string value, naming the environment
value, and an active expression (`tramp2-expression') to set it to.

If the value to set the variable to is `nil' the variable is unset
instead. To set an empty value, use \"\" as the value.

Values in this list override the TRAMP2 provided system default
values in `tramp2-shell-default-environment'.")


(defvar tramp2-remote-shell-path   '("/bin"
				     "/usr/bin"
				     "/usr/sbin"
				     "/usr/local/bin"
				     "/usr/ccs/bin"
				     "/local/bin"
				     "/local/freeware/bin"
				     "/local/gnu/bin"
				     "/usr/freeware/bin"
				     "/usr/pkg/bin")
  "*The directories to search for directories on the remote machine.")




;; REVISIT: This should be, like, 30 in the release. Short for debugging. :)
(defconst tramp2-timeout 1000
  "*Number of seconds to wait for a timeout.")

;; REVISIT: This should be (/ tramp2-timeout (if (featurep 'lisp-float-type) 10.0 10))
(defconst tramp2-timeout-short 0.3
  "Number of seconds to wait for a short timeout.
This value is used internally as the delay before checking more
input from remote processes and so forth.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug/progress message support.

;; REVISIT: This should be, like, zero or something.
(defvar tramp2-verbose 10
  "How verbose tramp2 should be about it's progress...")

(defun tramp2-message (level &rest args)
  "Display a message if LEVEL > `tramp2-verbose'."
  (when (> tramp2-verbose level)
    (apply 'message args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The hooks into the XEmacs file handlers.
;; This is the initial entry-point to the handlers. Almost all access to
;; tramp functionality will go through this routine.
(defun tramp2-file-name-handler (operation &rest args)
  "tramp2 file name handler.
This is invoked when a file operation is performed on a tramp2 file.
It locates the handler for the function, parses the file path into a
tramp2 path object and then calls the handler function with the
appropriate arguments."
  (let ((handler (tramp2-find-handler-for operation)))
    (if handler
	(apply handler args)
      (tramp2-call-without-handler operation args))))


;; Based on the original tramp function. 
(defun tramp2-call-without-handler (operation args)
  "Invoke normal file name handler for OPERATION.
This inhibits EFS and Ange-FTP, too, because they conflict with tramp."
  (let ((inhibit-file-name-handlers
         (list 'tramp-file-name-handler
	       'efs-file-handler-function
               'ange-ftp-hook-function
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines to manage the database of supported handlers.
(defun tramp2-find-handler-for (name)
  "Find the tramp2 operation handler for NAME in the database."
  (cdr-safe (assoc name tramp2-handler-alist)))


(defmacro def-tramp-handler (name args doc &rest body)
  "Define a new filename handler for tramp2.
NAME is the operation name and ARGS is the list of arguments that it
accepts. DOC will be used as the docstring.

This will define a suitable handler function for the file operaton,
ensure that it is inserted in the file handler list and ensure that
any filename arguments are parsed correctly into tramp2 paths before
the handler is called.

The symbol name `file' is magic in the ARGS. The first occurrance of
this name is treated as the filename parameter to the handler. This
will be automatically converted from the string representation to
a tramp2 path object."

;For example, defining a handler for `file-exists-p' would be:

;(def-tramp-handler file-exists-p (file)
;  \"Like `file-exists-p' for tramp files.\"  
;  ;; This is rather silly. :)  
;  (> 0 (random 1)))"
  (let ((fn-symbol  (intern (concat "tramp2-handler-for-" (symbol-name name))))
	(file-magic (member 'file args))
	(fn	    nil))

    ;; Add it's record to the handler list.
    (setq tramp2-handler-alist (remassoc name tramp2-handler-alist))
    (add-to-list 'tramp2-handler-alist (cons name fn-symbol))

    ;; Build the function declaration, including the magic
    ;; parsing of `file' if we need it...
    (append (list 'defun fn-symbol args doc)
	    (if file-magic
		`((let ,@(append
			  '(((file (cond ((tramp2-path-p file) file)
					 ((stringp file) (tramp2-path-parse file))
					 (t (signal-error 'tramp2-file-error
							  (list "Invalid path" file)))))))
			  body)))
	      body))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscelaneous support and compatibility routines.
(defun tramp2-find-value (user host data &optional default)
  "Find the appropriate data value in a tramp2 alist.
The list is searched for a number of keys, specifically:
  * (user . host)
  * host
  * 'default

If none of these are matched, the optional DEFAULT is returned."
  (or (car-safe (cdr-safe (or (assoc (cons user host) data)
			      (assoc host             data)
			      (assoc 'default         data))))
      default))
		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for the default protocols
(defun tramp2-ssh-make-command (user host)
  "Return a command string suitable for running ssh to a remote machine."
  (format "%s %s %s %s"
	  (tramp2-find-value user host tramp2-ssh-executable "ssh")
	  (tramp2-find-value user host tramp2-ssh-arguments "")
	  (if user (format "-l %s" user) "")
	  (or host "localhost")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell setup support.
(defun tramp2-shell-prompt (user host)
  "Return a regular expression to match a shell prompt on a remote machine.
This is drawn from the `tramp2-shell-prompt-pattern' alist, against the
cons of (user . host), then host alone, then `default'."
  (tramp2-find-value user host tramp2-shell-prompt-pattern shell-prompt-pattern))


(defun tramp2-setup-interactive-shell (connect path)
  "Establish an interactive shell on the remote system.
This is prerequisite to any other activity. Don't move this from
being the first setup function on the remote machine.

This does it's best to get a bourne shell that expands '~' running
as an interactive login."
  (let* ((user (tramp2-connect-user connect))
	 (host (tramp2-connect-host connect))
	 (shell (tramp2-find-value user host tramp2-remote-shell-alist "/bin/sh -i"))
	 found)
    ;; Search for a shell supporting tilde expansion...
    (setq found (catch 'found-shell
		  ;; Do we have a list of shells?
		  (if (listp shell)
		      (let ((shells shell)
			    shell)
			(while shells
			  (setq shell (car shells)
				shells (cdr shells))
			  (when (tramp2-setup-interactive-shell-test user shell)
			    (throw 'found-shell shell))))
		    (when (tramp2-setup-interactive-shell-test user shell)
		      (throw 'found-shell shell)))))
    ;; Did we actually find one?
    (if found
	(progn
	  ;; Replace the running shell with one that supports tilde expansion.
	  (tramp2-send-command-internal (format "exec %s" found))
	  ;; Turn off the display of our command...
	  (tramp2-send-command-internal "stty -echo -ocrnl")
	  ;; Resync with the remote shell...
	  (unless (tramp2-run-actors (get-buffer-process (current-buffer))
				     tramp2-shell-startup-actors)
	    (if (eq 'run (process-status nil))
		(signal-error 'tramp2-file-error '("Remote host timed out")))
	    (signal-error 'tramp2-file-error '("Remote host closed connection"))))
      ;; Failed to find a suitable shell...
      (signal-error 'tramp2-file-error
		    '("Unable to find shell supporting tilde '~' expansion"
		      shell connect)))))

      
(defun tramp2-setup-interactive-shell-test (user shell)
  "Run SHELL on the remote machine and test if it supports tilde
expansion. Return the success or failure of that test.

We assume that USER, the user we loged in with, has a home
directory on the machine. If that user does not exist we presume
that the remote machine is Unix-alike and use \"root\".

This routine DOES NOT leave the remote shell running even
if it does support tilde expansion. This is because we want the
shell to exit and take down the whole connection later on..."
  (save-match-data
    (let ((user (or user "root")))
      ;; Execute the particular shell on the remote machine. Note that
      ;; we don't destroy the connection if the shell fails to exist.
      (tramp2-send-command-internal shell)
      ;; Resync with the remote shell...
      (unless (tramp2-run-actors (get-buffer-process (current-buffer))
				 tramp2-shell-startup-actors)
	(if (eq 'run (process-status nil))
	    (signal-error 'tramp2-file-error '("Remote host timed out")))
	(signal-error 'tramp2-file-error '("Remote host closed connection")))

      ;; The shell has made it to an interactive prompt. Now we want to
      ;; talk to it and determine if it actually does what we want...
      (unless (= 0 (tramp2-send-command (format "echo ~%s" user)))
	(signal-error 'tramp2-file-error '("echo ~root failed, very odd!" shell)))
      ;; Return result of tilde expansion test...
      (let ((result (not (search-forward-regexp (format "^~%s" user) nil t))))
	;; Make the test shell exit...
	(tramp2-send-command-internal "exit")
	;; Return the result.
	result))))


(defun tramp2-setup-remote-environment (connect path)
  "Configure the remote environment for a tramp2 shell session."
  (let ((sys-env tramp2-shell-default-environment)
	(user-env (tramp2-find-value (tramp2-connect-user connect)
				     (tramp2-connect-host connect)
				     tramp2-shell-environment))
	env)
    ;; Walk through the system environment.
    (while sys-env
      (setq env (car sys-env)
	    sys-env (cdr sys-env))
      ;; Does this entry exist in the user-env?
      (unless (assoc (car env) user-env)
	;; Nope, set it.
	(tramp2-setup-remote-environment-set connect env)))
    ;; Walk through the user environment.
    (while user-env
      (setq env (car user-env)
	    user-env (cdr user-env))
      (tramp2-setup-remote-environment-set connect env))))


(defun tramp2-setup-remote-environment-set (connect env)
  "Set (or unset) the value specified in ENV for connection CONNECT."
  (let ((name (car env))
	(val  (tramp2-expression (cadr env) connect)))
    (unless (or (= 0 (tramp2-send-command (format (if val "export %s='%s'" "unset %s")
						  name val)))
		(not val))
      (signal-error 'tramp2-file-error (list "Failed to set value" name val)))))

(defun tramp2-shell-path (user host)
  "Return the remote search path to use for a connection."
  (mapconcat #'identity tramp2-remote-shell-path ":"))


(defun tramp2-setup-file-transfer (connect path)
  "Configure the remote file transfer encoding."
  (let* ((protocol (tramp2-connect-protocol connect))
	 (encoding (or (and (symbolp protocol)
			    (tramp2-protocol-get 'encoding protocol))
		       (tramp2-setup-file-transfer-autodetect connect path))))
    (unless encoding
      (signal-error 'tramp2-file-error (list "No valid encoding" path)))
    (tramp2-setup-file-transfer-install encoding)))


(defun tramp2-setup-file-transfer-autodetect (connect path)
  "Automatically detect a suitable transfer encodinging for the
given path and connection."
  (catch 'found
    (let ((encodings tramp2-encoding-alist)
	  encoding)
      (while encodings
	(setq encoding  (car encodings)
	      encodings (cdr encodings))
	(let ((name (car encoding))
	      (test (assoc 'test (cdr encoding))))
	  (when test
	    (when (funcall (cdr test) connect path)
	      (throw 'found name)))))
      nil)))


(defun tramp2-setup-file-transfer-install (encoding)
  "Install ENCODING as this buffers encoding type."
  (let ((data (cdr-safe (assoc encoding tramp2-encoding-alist))))
    (unless (and data
		 (assoc 'write data)
		 (assoc 'read  data))
      (signal-error 'tramp2-file-error (list "Poorly formed encoding" encoding)))
    (set (make-local-variable 'tramp2-write) (cdr (assoc 'write data)))
    (set (make-local-variable 'tramp2-read)  (cdr (assoc 'read  data)))))


(defun tramp2-write (start end file &optional append)
  "Write the region from START to END in the current buffer
to FILE. If APPEND is non-nil, append to the remote file rather
than overwriting it."
  ;; Step into the tramp2 connection buffer...
  (let ((source (current-buffer)))
    (tramp2-with-connection file
      (unless (fboundp 'tramp2-write)
	(signal-error 'tramp2-file-error (list "No write routine for buffer" file)))
      (funcall tramp2-write source start end file append))))
	       

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote command execution support.
(put 'tramp2-with-connection 'lisp-indent-function 1)

(defmacro tramp2-with-connection (path &rest body)
  ;; Make sure we have an established connection...
  `(let* ((path ,path)
	  (buffer (or (get-buffer (tramp2-buffer-name path))
		      (tramp2-buffer-create path))))
     (unless (and buffer (bufferp buffer))
       (signal-error 'tramp2-file-error (list "Failed to find/create buffer"
					      (tramp2-buffer-name path))))
     (with-current-buffer buffer
       (unless (tramp2-buffer-p)
	 (signal-error 'tramp2-file-error (list "Invalid buffer for connect"
						(tramp2-buffer-name path))))
       
       ;; Are we an established connection?
       (unless (eq tramp2-state 'connected)
	 ;; Establish the connection...
	 (let* ((setup tramp2-setup-functions)
		(hops (tramp2-path-connect path))
		(hop  (prog1
			  (car hops)
			(setq hops (cdr hops)))))
	   ;; Establish the first hop.
	   (unless (tramp2-run-hop 'tramp2-execute-local hop)
	     (signal-error 'tramp2-file-error
			   (list "Failed to make first connection"
				 (tramp2-buffer-name path) hop)))
	   ;; Advance to the next state.
	   (tramp2-set-buffer-state 'in-progress)
	   (while hops
	     ;; Establish the next hop...
	     (unless (tramp2-run-hop 'tramp2-execute-nexthop (prog1
								 (setq hop (car hops))
							       (setq hops (cdr hops))))
	       (signal-error 'tramp2-file-error
			     (list "Failed to make next connection"
				   (tramp2-buffer-name path) hop))))
	   
	   ;; Advance to the setup state.
	   (tramp2-set-buffer-state 'setup)
	   ;; Run the setup hooks.
	   (while setup
	     (funcall (prog1 (car setup) (setq setup (cdr setup))) hop path))
	   ;; Advance the state to connected.
	   (tramp2-set-buffer-state 'connected)))
       
       ;; Run the body of the thing...
       (progn . ,body))))



(defun tramp2-run-command (path command)
  "Execute COMMAND on the host specified by PATH.

This returns the return code of the command.
The current buffer is also changed to a buffer containing
the output of the command."
  (unless (and command
	       (stringp command)
	       (> (length command)) 0)
    (signal-error 'tramp2-file-error (list "Invalid or empty command" command)))
  (tramp2-with-connection path
    (tramp2-send-command command)))    



(defun tramp2-run-hop (fn connect)
  "Execute the command for CONNECT via FN and run any connect actions
that match the output of it."
  (let ((command (tramp2-connect-command connect)))
    (save-match-data
      ;; Get the remote command executed...
      (unless (= 0 (funcall fn command))
	(signal-error 'tramp2-file-error (list "Remote command failed" command)))
      (if (tramp2-run-actors (get-buffer-process (current-buffer))
			     tramp2-connect-actors)
	  t
	(if (eq 'run (process-status nil))
	    (signal-error 'tramp2-file-error (list "Remote host timed out" command))
	  (signal-error 'tramp2-file-error (list "Remote host closed connection" command)))))))


(defun tramp2-run-actors (proc actors)
  "Run remote connection actors until the connection is complete.
PROC is the process object for the remote connection.

This function depends on `point' in the connection buffer being a
valid point to start looking for remote connection output from.

ACTORS is a list of actions to take while connecting to a remote
machine. This is a list of (MATCH . ACTION) pairs, executed in order.

MATCH is a tramp2 active expression. The string it returns is treated
as a regular expression to match against.

This match is applied from the start of the line following a line
containing a previously successful match.

For example, \"<eom>\" is the end of the last match and the buffer
contains:

\"password: <eom>
 <start>next line\"

The next match will start at \"<start>\".

ACTION is a tramp active expression that returns a string to send
to the remote system.

This function returns `t' if one of the actions signaled `ready'
or `nil' if the connection failed or timed out."
  (unless (and proc (processp proc))
    (signal-error 'tramp2-file-error (list "Invalid connection" proc)))
  (unless (and (listp actors) (> (length actors) 0))
    (signal-error 'tramp2-file-error (list "Invalid action list" actors)))
  
  (with-current-buffer (process-buffer proc)
    ;; Now run the command handlers.
    (let ((from (point-marker)))
      ;; Loop until someone exits via `throw' or remote exits.
      (with-timeout (tramp2-timeout nil)
	(catch 'ready
	  (while (eq 'run (process-status proc))
	    ;; Jump to the last search position.
	    (goto-char from)
	    ;; Look for something to do.
	    (tramp2-message 5 "Looking for remote event...")
	    (let ((actions actors))
	      (while actions
		(setq act     (car actions)
		      actions (cdr actions))
		(when (search-forward-regexp (tramp2-expression (car act) connect) nil t)
		  (tramp2-message 5 "Looking for remote event... responding.")
		  ;; Send the action
		  (tramp2-send-command-internal
		   (tramp2-expression (cdr act) connect))
		  ;; Sync to the start of the next line.
		  (goto-char (match-end 0))
		  (tramp2-message 5 "Looking for start of next line...")
		  (while (not (search-forward-regexp "^." nil 'end))
		    (accept-process-output proc tramp2-timeout-short))
		  ;; Remember where we are.
		  (tramp2-message 5 "Looking for start of next line... found.")
		  (goto-char (match-beginning 0))
		  (setq from (point-marker)))))
	    ;; Fetch more output
	    (tramp2-message 5 "Waiting for more input...")
	    (accept-process-output proc tramp2-timeout-short)))))))
  
    

(defun tramp2-send-command-internal (command)
  "Send COMMAND to the current remote connection.
The command is automatically terminated with the appropriate
line-ending if it is not already.

You don't want to use this for most things. If does no error
checking on the remote command and does not return the
exit status of the remote command.

See `tramp2-send-command' for a vastly more useful routine
for remote command processing."
  (unless (tramp2-buffer-p)
    (signal-error 'tramp2-file-error (list "Invalid buffer for command" command)))
  (save-match-data
    (process-send-string nil (if (string-match "\n$" command)
				 (progn
				   (debug)
				   command)
			       (concat command "\n")))))


(defvar tramp2-bracket-string-count 0
  "Number of bracketing strings that have been generated.
This is a counter used to ensure that the generated string is
unique within this Emacs process.")

(defun tramp2-make-bracket-string ()
  "Generate a string that is relatively unique. The output shouldn't
occur in any output from any shell command anywhere on the planet,
nor should it be similar to the previously generated strings.

The string also has substitution points for indicating a start
or end string and a return code."
  (format "-=-=-=-=- tramp2 command %%s bracket %x %x %x: %%s : -=-=-=-=-"
	  (setq tramp2-bracket-string-count (1+ tramp2-bracket-string-count))
	  (mod (apply 'logxor (current-time)) (emacs-pid))
	  (apply 'logxor (current-time))))

	  
(defun tramp2-send-command (command)
  "Send COMMAND to the remote host and await it's completion.
When it exits, it's exit status is returned and the buffer
contains the output of the process.

Any other text contained within the buffer is removed by this call;
don't make it if you need the current content unless you preserve it
through some other mechanism.

If you *really* need to send a command without blocking - which you
don't need to do, let me assure you - see `tramp2-send-command-internal'."
  (save-match-data
    ;; Get hold of a string suitable for bracketing remote output.
    (let* ((raw-bracket (tramp2-make-bracket-string))
	   (start   (format "echo \"%s\"" (format raw-bracket "start" "$$")))
	   (exit     (format "echo \"%s\"" (format raw-bracket "end" "$?")))
	   (start-re (concat (format raw-bracket "start" "[0-9]+") "\n"))
	   (exit-re  (concat (format raw-bracket "end" "\\([0-9]+\\)") "\n"))
	   (retval -1))
      ;; Erase the old buffer content.
      (erase-buffer)
      ;; Throw the command at the remote shell.
      (tramp2-send-command-internal (format "%s; { %s }; %s"
					    start
					    (if (string-match "\n$" command)
						command
					      (concat command "\n"))
					    exit))
      ;; Spin until the output shows up.
      (with-timeout (tramp2-timeout '(signal-error 'tramp2-file-error
						   (list "Remote host timed out" command)))
	(while (not (search-forward-regexp exit-re nil t))
	  (accept-process-output (get-buffer-process (current-buffer))
				 tramp2-timeout-short)
	  (goto-char (point-min))))

      ;; We found the output...
      (goto-char (match-beginning 1))
      ;; Read in the value.
      (setq retval (read (current-buffer)))

      ;; Remove the trailer from the buffer...
      (delete-region (match-beginning 0) (point-max))
      ;; Find the leader
      (goto-char (point-min))
      (search-forward-regexp start-re nil t)
      ;; Remove it...
      (delete-region (point-min) (match-end 0))
      ;; And the extranious newline that sneaks in...
      ;; REVISIT: This is possibly wrong. Verify that this routing *is* the
      ;; cause of the extra newline that shows up.
      (when (looking-at "^$")
	(kill-line 1))

      ;; Return the result of the command being executed.
      retval)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote command drivers for the various connection stages.
(defun tramp2-execute-local (command)
  "Execute COMMAND in the context of a local Emacs.
This automatically advances the connection state to `in-progress'."
  (let ((buffer (current-buffer))
	(process-connection-type t)	; need a tty for ssh. :/
	proc)
    (unless (and (eq tramp2-state 'disconnected)
		 (null (get-buffer-process buffer)))
      (signal-error 'tramp2-file-error
		    (list "Local command in non-disconected buffer" command)))
    ;; Start the process.
    (setq proc (start-process-shell-command "tramp remote shell" (current-buffer) command))
    ;; Set a notification handler for connection failure...
    (set-process-sentinel proc #'tramp2-execute-local-sentinel)
    ;; Allow the process to silently die at the end of the Emacs session.
    (process-kill-without-query proc)
    ;; If the process exited before the sentinal was in place
    ;; - this does happen, you know - we need to handle that.
    (unless (eq (process-status proc) 'run)
      (tramp2-set-buffer-state 'disconnected))
    ;; Return process exit status - or 0 to show success.
    (process-exit-status proc)))
  
(defun tramp2-execute-local-sentinel (proc change)
  "Handle changes in process status for local connections."
  (let ((status (process-status proc))
	(buffer (process-buffer proc)))
    ;; If it's not running...
    (unless (eq status 'run)
      ;; If it's not a clean exit (urgh!)
      (when (process-live-p proc)
	(kill-process proc))
      (when (buffer-live-p buffer)
	(with-current-buffer buffer
	  (tramp2-set-buffer-state 'disconnected)
	  (erase-buffer))))))



(defun tramp2-execute-nexthop (command)
  "Execute COMMAND to establish the next hop in a connection chain.
This *must* ensure that when the next-hop connection command exits,
the shell also exits and the connection is closed.

This is typically done by using 'exec' on the remote shell."
  (let ((proc (get-buffer-process (current-buffer))))
    (unless (and (tramp2-buffer-p)
		 (eq tramp2-state 'in-progress)
		 (not (null proc)))
      (signal-error 'tramp2-file-error (list "Next-hop command in bad buffer" command)))

    ;; Jumping to the next host is trivial, just exec the command...
    (tramp2-send-command-internal (format "exec %s" command))
    ;; Return the process status; if it's dead, something went wrong.
    (process-exit-status proc)))
    



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection buffer management.
(defun tramp2-buffer-name (path)
  "Return the buffer name that manages the tramp2 connection for PATH."
  (format "* tramp connection for: %s *"
	  (mapconcat 'tramp2-connect-to-string (tramp2-path-connect path) " / ")))


(defun tramp2-buffer-p (&optional buffer)
  "Determine if BUFFER is a tramp2 connection buffer.
The current buffer is used if none is specified."
  (local-variable-p 'tramp2-state (or buffer (current-buffer))))


;; Create a new connection buffer for a connection.
(defun tramp2-buffer-create (path)
  "Create a connection buffer for PATH in the `disconnected' state."
  (let ((buffer (get-buffer-create (tramp2-buffer-name path))))
    (condition-case nil
	(with-current-buffer buffer
	  (tramp2-set-buffer-state-internal 'disconnected)
	  (unless (tramp2-buffer-p)
	    (signal-error 'tramp2-file-error (list "Creating buffer failed" path))))
      (t (kill-buffer buffer)))
    buffer))


(defun tramp2-set-buffer-state (state)
  "Set the state of the connection for PATH to STATE.
STATE must be one of the known connection states:

* `disconnected'
  The buffer process does not exist and there is no connection to any
  process, local or remote.

* `in-progress'
  The buffer process is connected to an intermediate machine
  (or connection) between the local host and the final host.

* `setup'
  The buffer process is connected to the final shell but is not yet
  ready to run tramp2 commands remotely.

* `connected'
  The buffer is connected to the final shell and is ready to run
  remote commands.

This routine makes the changes to the buffer state atomically but does
not actually execute the commands required to achieve any transition.
It should be called *after* a state is transitioned to, not before.

Valid transitions are:
disconnected => in-progress | setup
in-progress  => setup
setup        => connected

connected    => disconnected

Note that it is an error to transition from the `in-progress' or `setup'
states to disconnected."
  (let ((buffer (current-buffer)))
    (unless (tramp2-buffer-p buffer)
      (signal-error 'tramp2-file-error (list "State change on unexisting connection" path)))

    (with-current-buffer buffer
      ;; Ensure that we are in a valid state.
      (unless (or (eq tramp2-state 'disconnected)
		  (eq tramp2-state 'in-progress)
		  (eq tramp2-state 'setup)
		  (eq tramp2-state 'connected))
	(signal-error 'tramp2-file-error (list "Connection state invalid" path tramp2-state)))

      ;; Ensure that the transition is valid.
      (when (or (and (eq state 'in-progress) (not (eq tramp2-state 'disconnected)))
		(and (eq state 'setup)   (not (or (eq tramp2-state 'disconnected)
						  (eq tramp2-state 'in-progress))))
		(and (eq state 'connected)   (not (eq tramp2-state 'setup))))
	(signal-error 'tramp2-file-error (list "Bad state transition" path tramp2-state state)))
      
      ;; Actually do the work of the transition.
      (tramp2-set-buffer-state-internal state))))


(defun tramp2-set-buffer-state-internal (state)
  "Set the tramp2 connection in BUFFER to STATE.
This does no validation of the state. See `tramp2-set-connection-state'."
    ;; Record the new state of the buffer.
    (set (make-local-variable 'tramp2-state) state))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation of the "active expression" stuff.
;; This is the worker that massages an active expression into the string
;; it should be.

(defun tramp2-expression (expression connect)
  "Convert EXPRESSION as an active expression using CONNECT.

An active expression is:
  * a symbol
    The symbol will be called as a function. The arguments `user' and
    `host' will be passed to the function. It should return a string
    to be used for the expression.

  * a function
    The function will be called with arguments `user' and `host'.
    It should return a string to be used for the expression.

  * a list
    The list will be evaluated. The variables `user' and `host' will
    be the current user and host. It should return a string to be used
    for the expression.

  * a string
    The string will used directly after substitutions occur.
    `%u' will be replaced by the user and `%h' by the host.
    These will be replaced with an empty string if there is no
    value."

  (cond ((and (symbolp expression) (fboundp expression))
	 (funcall expression
		  (tramp2-connect-user connect)
		  (tramp2-connect-host connect)))

	((functionp expression)
	 (funcall expression
		  (tramp2-connect-user connect)
		  (tramp2-connect-host connect)))

	((listp expression)
	 (let ((user (tramp2-connect-user connect))
	       (host (tramp2-connect-host connect))
	       (code expression))
	   (if (listp (car-safe expression))
	       (while code
		 (eval (prog1
			   (car code)
			 (setq code (cdr code)))))
	     (eval expression))))
	 
	((stringp expression)
	 (save-match-data
	   (let ((text expression)
		 (user (tramp2-connect-user connect))
		 (host (tramp2-connect-host connect)))
	     (while (string-match "%u" text)
	       (setq text (replace-match user t t text)))
	     (while (string-match "%h" text)
	       (setq text (replace-match host t t text)))
	     text)))))
		  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol database managment.
(defun tramp2-protocol-get (protocol property)
  "Return the PROPERTY value of PROTOCOL, or `nil' if there is no
such property."
  (cdr-safe (assoc property (assoc protocol tramp2-protocol-alist))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path object management.
;; This defines a tramp2 path object and the constructor and accessor for
;; such a beast.
(defun tramp2-path-p (path)
  "Determine if the object presented is a well-formed TRAMP2 path object."
  (and (vectorp path)
       (eq 'tramp2-path-struct-tag (elt path 0))))

(defun make-tramp2-path (connect path)
  "Create a well-formed tramp2 path object."
  (unless (and (listp connect)
	       (listp (car connect)))
    (signal-error 'tramp2-file-error (list "Invalid connection" connect)))
  (vector 'tramp2-path-struct-tag
	  connect
	  path))

(defun tramp2-path-connect (path)
  "Return the connect set for a tramp2 path."
  (elt path 1))

(defun tramp2-path-remote-file (path)
  "Return the file path on the remote machine, from a tramp2 path."
  (elt path 2))


(defun tramp2-connect-p (connect)
  "Determine if an object is a well-formed tramp2 connect object."
  (and (listp connect)
       (eq 'tramp2-connect-struct-tag (nth 0 connect))))

(defun make-tramp2-connect (protocol user host)
  "Create a tramp2 connect object."
  (unless (or (null protocol)
	      (symbolp protocol)
	      (stringp protocol))
    (signal-error 'tramp2-file-error (list "Invalid protocol in connect" protocol)))
  (unless (or (null user)
	      (stringp user))
    (signal-error 'tramp2-file-error (list "Invalid user in connect" protocol)))
  (unless (or (null host)
	      (stringp host))
    (signal-error 'tramp2-file-error (list "Invalid host in connect" host)))
  (unless (or user host)
    (signal-error 'tramp2-file-error (list "Connect requires one of USER or HOST")))
  (list 'tramp2-connect-struct-tag protocol user host))

(defun tramp2-connect-protocol (connect)
  "Get the protocol identifier used for a connection."
  (nth 1 connect))

(defun tramp2-connect-user (connect)
  "Get the user identifier for a connect."
  (nth 2 connect))

(defun tramp2-connect-host (connect)
  "Get the host identifier for a connect."
  (nth 3 connect))

(defun tramp2-connect-to-string (connect)
  "Return a stringified version of a connect statement."
  (unless (tramp2-connect-p connect)
    (signal-error 'tramp2-file-error (list "Not a tramp2 connect list" connect)))
  (let ((protocol (tramp2-connect-protocol connect))
	(user     (tramp2-connect-user connect))
	(host     (tramp2-connect-host connect)))
    (concat (cond ((null protocol)    nil)
		  ((symbolp protocol) (concat (symbol-name protocol) "|"))
		  ((stringp protocol) (concat "[" protocol "]")))
	    (when user (concat user "@"))
	    host)))

(defun tramp2-connect-command (connect)
  "Return a fully expanded string specifying the command to run for
a given path."
  (let ((command (or (tramp2-connect-protocol connect)
		     tramp2-default-protocol)))
    (tramp2-expression (if (stringp command)
			   command
			 (tramp2-protocol-get command 'command))
		       connect)))

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path string parser (and validator)
(defun tramp2-path-parse (the-path)
  "Parse a TRAMP2 path string into a structure defining the connections
and the remote file path required."
  (save-match-data
    (unless (and (stringp the-path)
		 (string-match (concat "^" tramp2-path-tag) the-path))
      (signal-error 'tramp2-file-error (list "Not a TRAMP2 path" the-path)))
    ;; Extract the connection expressions.
    (let ((case-fold-search 	t)
	  (path    		(substring the-path (match-end 0)))
	  (connect 		nil))
      (catch 'end
	(while (string-match tramp2-path-connect path)
	  (unless (or (match-string 5 path) (match-string 6 path))
	    (throw 'end nil))
	  (setq connect (append connect
				(list
				 (make-tramp2-connect (or (match-string 2 path)
							  (and (match-string 3 path)
							       (intern (match-string 3 path))))
						      (match-string 5 path)
						      (match-string 6 path))))
		path    (substring path (match-end 0)))))
      (unless (string-match "^:.*$" path)
	(signal-error 'tramp2-file-error (list "Not a TRAMP2 path" the-path)))
      (make-tramp2-path connect (substring path 1)))))


(defun tramp2-path-construct (the-path)
  "Build a string version of a tramp path."
  (unless (tramp2-path-p the-path)
    (signal-error 'tramp2-file-error (list "Not a TRAMP2 path" the-path)))
  (concat tramp2-path-tag
	  (mapconcat 'tramp2-connect-to-string (tramp2-path-connect the-path) ":")
	  "::" (tramp2-path-remote-file the-path)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note that we exist, populate our file ops, run user hooks (if any)
(provide 'tramp2)

;; Load other components of the TRAMP2 code.
(require 'tramp2-enc)
(require 'tramp2-ops)

;; Run any user hooks.
(run-hooks 'tramp2-load-hooks)


;; TODO:
;; * Port the MULE support from TRAMP for the connection process.
;;   See especially `tramp2-send-command' and `tramp2-execute-local'.
;;
;; * Port handlers from TRAMP to TRAMP2 (in tramp2-ops.el, please).
;;   See `def-tramp-handler' for details on writing a handler.
;;
;; * Is cl really required? FSF Emacs hate it with a passion, for some
;;   reason that I cannot fathom, and so prefer it not be required at
;;   runtime by any code... which we are doing.
;;
;; * Populate `tramp2-connect-actors'. This needs:
;;   - password prompt handling
;;   - login name handling
;;   - tset/terminal type prompting
;;
;; * Progress messages need to be refined.
;;   - Work out what level various events sit at and define constants.
;;   - Write progress messages for the code.
;;
;; * Better error handling.
;;   - Define a timeout error and a helper to throw one.
;;   - Throw human-readable error messages everywhere.
;;
;; * We should auto-load the encodings and so forth to avoid bloating
;;   the image.
;;
;; * Support for file-locking. This only requires the existence of
;;   symlink creation and testing predecates. We /should/ be able
;;   to do it. If not XEmacs (and Emacs if we have an enthusiast)
;;   should be patched to support it.


;;; tramp2.el ends here
