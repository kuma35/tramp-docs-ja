;;; tramp2.el --- Network file access via login shell

;; Copyright (C) 2001 by Daniel Pittman <daniel@rimspace.net>

;;; Commentary:

;; Based on the TRAMP code by Kai Großjohann, et al.

;;; Code:

;; Error thrown when a file is invalid.
(define-error 'tramp2-file-error
  "Error thrown when a tramp2 specific error occurs.
Inheritance ensures that anything expecting generic file errors will be happy."
  'file-error)


(defvar tramp2-load-hooks nil
  "*Hooks run when the tramp2 file interface is loaded.")

(defconst tramp2-path-tag "/!:"
  "Regular expression matching a TRAMP2 path tag.")

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

(defvar tramp2-handler-alist nil
  "Associative list of tramp2 file operation handlers.
To define a file operation handler, see the `defhandler' macro.")

;; REVISIT: What goes here?
(defvar tramp2-default-protocol nil
  "The default protocol to use.")

;; REVISIT: Fill this in.
(defvar tramp2-protocol-alist '(nil ((command nil)))
  "An associative set of protocol tags, each mapping to an alist
defining the characteristics of the connection.

Each protocol has a symbol as a tag. The defined characteristics are:

* `command', the command line to execute. A tramp2 active expression.
  See `tramp2-expression' for more details.")


;; Internal valiable, use this?
(defconst tramp2-state-alist '((disconnected ((tramp2-execute tramp2-execute-local)))
			       (in-progress  ((tramp2-execute tramp2-execute-nexthop)))
			       (setup        ((tramp2-execute tramp2-execute-setup)))
			       (connected    ((tramp2-execute tramp2-execute-remote))))
  "State transition data for the tramp2 connection engine.
Don't modify this unless you know what you are doing!")


(defvar tramp2-setup-functions '(nil)
  "The list of functions to run, in order, to setup the remote shell.
This is run in the tramp2 connection buffer and should run commands
to ensure that the remote shell is ready to accept commands.")


(defconst tramp2-timeout 30
  "Number of seconds to wait for a timeout.")




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
;; Remote command execution support.
(defun tramp2-run-command (path command)
  "Execute COMMAND on the host specified by PATH.
ARGS are used as the arguments to the command.

This returns the return code of the command. The output of the
process is left in the buffer."
  (unless (and command
	       (stringp command)
	       (> 0 (length command)))
    (signal-error 'tramp2-file-error (list "Invalid or empty command" path command)))
  (let* ((buffer-name (tramp2-buffer-name path))
	 (buffer      (or (get-buffer buffer-name)
			  (tramp2-buffer-create path))))
    (unless (and buffer (bufferp buffer))
      (signal-error 'tramp2-file-error (list "Failed to find/create buffer" path)))
    (with-current-buffer buffer
      (unless (tramp2-buffer-p)
	(signal-error 'tramp2-file-error (list "Invalid buffer for connect" path)))

      ;; Are we an established connection?
      (unless (eq tramp2-state 'connected)
	;; Establish the connection...
	(let ((hops (tramp2-path-connect path))
	      hop)
	  (while hops
	    ;; Extract the next hop to make.
	    (setq hop (car hops)
		  hops (cdr hops))
	    ;; Run the command it specifies.
	    (apply tramp2-execute (tramp2-connect-command hop)))
	  ;; Advance to the setup state.
	  (tramp2-set-buffer-state 'setup)
	  ;; Run the setup hooks.
	  (mapc #'funcall tramp2-setup-functions)
	  ;; Advance the state to commected.
	  (tramp2-set-buffer-state 'connected)))

      ;; Established connection, run the command.
      (apply tramp2-execute command))))


(defun tramp2-send-command (command)
  "Send COMMAND to the current remote connection.
The command is automatically terminated with the appropriate
line-ending."
  (unless (tramp2-buffer-p)
    (signal-error 'tramp2-file-error (list "Invalid buffer for command" command)))
  (process-send-string nil (concat command "\n")))


(defun tramp2-execute-local (command)
  "Execute COMMAND in the context of a local Emacs.
This automatically advances the connection state to `in-progress'."
  (let ((buffer (current-buffer))
	(process-connection-type nil)
	proc)
    ;; DEBUG
    (unless (and (eq tramp2-state 'disconnected)
		 (null (get-buffer-process buffer)))
      (signal-error 'tramp2-file-error
		    (list "Local command in non-disconected buffer" command)))
    ;; Start the process.
    (setq proc (apply 'start-process "tramp remote shell" (current-buffer)
		      command args))
    (set-process-sentinel proc #'tramp2-execute-local-sentinel)
    ;; Advance the connection state to in-progress.
    (tramp2-set-buffer-state 'in-progress)
    ;; Return process exit status - or 0 if it's running, to show success.
    (process-exit-status proc)))
  
(defun tramp2-execute-local-sentinel (proc change)
  "Handle changes in process status for local connections."
  (let ((status (process-status proc))
	(buffer (process-buffer proc)))
    ;; If it's not running...
    (unless (eq status 'run)
      ;; If it's not a clean exit (urgh!)
      (unless (eq 'status 'exit) 
	(kill-process proc))
      (when (and buffer (bufferp buffer))
	(with-current-buffer buffer
	  (tramp2-set-buffer-state 'disconnected))))))



(defun tramp2-execute-nexthop (command)
  "Execute COMMAND to establish the next hop in a connection chain.
This *must* ensure that when the next-hop connection command exits,
the shell also exits and the connection is closed.

This is typically done by using 'exec' on the remote shell."
  ;; DEBUG
  (unless (and (eq tramp2-state 'in-progress)
	       (not (null (get-buffer-process buffer))))
    (signal-error 'tramp2-file-error
		  (list "Next-hop command in bad buffer" command)))
  (signal-error 'tramp2-file-error (list "REVISIT: Not Implemented" path)))


(defun tramp2-execute-setup (command) 
  "Execute COMMAND to perform a remote setup task on the final host."
  (signal-error 'tramp2-file-error (list "REVISIT: Not Implemented" path)))

(defun tramp2-execute-remote (command) 
  "Execute COMMAND on a fully configured remote machine."
  (signal-error 'tramp2-file-error (list "REVISIT: Not Implemented" path)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection buffer management.
(defun tramp2-buffer-name (path)
  "Return the buffer name that manages the tramp2 connection for PATH."
  (format "* tramp connection for: %s *"
	  (mapconcat 'tramp2-connect-to-string (tramp2-path-connect path) " / ")))


(defun tramp2-buffer-p (&optional buffer)
  "Determine if BUFFER is a tramp2 connection buffer.
The current buffer is used if none is specified."
  (with-current-buffer (or buffer (current-buffer))
    (and (local-variable-p 'tramp2-state)
	 (local-variable-p 'tramp2-execute)
	 (fboundp tramp2-execute))))


;; Create a new connection buffer for a connection.
(defun tramp2-buffer-create (path)
  "Create a connection buffer for PATH in the `disconnected' state."
  (let ((buffer (get-buffer-create (tramp2-buffer-name path))))
    (with-current-buffer buffer
      (tramp2-set-buffer-state-internal 'disconnected)
      (unless (tramp2-buffer-p)
	(signal-error 'tramp2-file-error (list "Creating buffer failed" path))))))


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
  (let ((data (assoc state 'tramp2-state-alist)))
    ;; Process the data from the state transition.
    (while data
      (set (make-local-variable (car data)) (cadr data))
      (setq data (cdr data)))
    
    ;; Record the new state of the buffer.
    (set (make-local-variable 'tramp2-state) state)))



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
	       (host (tramp2-connect-host connect)))
	   (eval expression)))

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

(defun tramp2-path-remote-path (path)
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
  (tramp2-expression (or (tramp2-connect-protocol connect)
			 tramp2-default-protocol)
		     connect))
    

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
	  "::" (tramp2-path-remote-path the-path)))



;; Drag in the file operation handlers.
;(require 'tramp2-ops)

(run-hooks 'tramp2-load-hooks)

(provide 'tramp2)

;; TODO:
;; * Port the MULE support from TRAMP for the connection process.
;;   See especially `tramp2-send-command' and `tramp2-execute-local'.
;;
;; * Port handlers from TRAMP to TRAMP2 (in tramp2-ops.el, please).
;;   See `def-tramp-handler' for details on writing a handler.

;;; tramp2.el ends here
