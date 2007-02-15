;;; -*- coding: iso-8859-1; -*-
;;; tramp-gw.el --- Tramp utility functions for HTTP and SOCKS gateways

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
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

;; Access functions for HTTP and SOCKS gateways from Tramp.

;;; Code:

(require 'tramp)
(require 'url-http)

;; Pacify byte-compiler
(eval-when-compile
  (require 'cl)
  (require 'custom))

;; Avoid byte-compiler warnings if the byte-compiler supports this.
;; Currently, XEmacs supports this.
(eval-when-compile
  (when (featurep 'xemacs)
      (byte-compiler-options (warnings (- unused-vars)))))

;; Define GATEWAY method ...
(defvar tramp-gw-method "gw"
  "*Method to connect HTTP gateways.")

;; ... port.
(defvar tramp-gw-default-port 8080
  "*Default port for HTTP gateways.")

;; Add a default for `tramp-default-user-alist'. Default is the local user.
(add-to-list 'tramp-default-user-alist
	     `(,tramp-gw-method nil ,(user-login-name)))

;; Internal file name functions and variables.

;; `tramp-gw-current-*' variables are only used for creation of the
;; connection.  They can be overwritten for every new connection
;; request.
(defvar tramp-gw-current-vector nil
  "Keeps the remote host identification.")

(defvar	tramp-gw-current-gw-vector nil
  "Keeps the gateway identification.")

(defvar tramp-gw-current-http-proc nil
  "Current URL-HTTP process.")

(defvar tramp-gw-current-http-buffer nil
  "Current URL-HTTP buffer.")

;; This variable keeps the listening process, in order to reuse it for
;; new processes.
(defvar tramp-gw-aux-proc nil
  "Process listening on local port, as mediation between SSH and HTTP.")

(defun tramp-gw-make-urlobj (vec)
  "Convert Tramp VEC into a vector url syntax used in url.el.
Localname of VEC must be the target to be connected in \"host:port\" syntax.
Result is \[TYPE USER PASSWORD HOST PORT FILE TARGET ATTRIBUTES FULL\]."
  (let ((host (tramp-file-name-host vec))
	(user (tramp-file-name-user vec))
	(localname (tramp-file-name-localname vec)))
    (string-match "\\([^#]+\\)\\(#\\(.+\\)\\)?" host)
    (let ((result (make-vector 9 nil)))
      (url-set-type result "http")
      (url-set-user result user)
      (url-set-host result (match-string 1 host))
      (url-set-port
       result
       (if (match-string 3 host)
	   (string-to-number (match-string 3 host))
	 tramp-gw-default-port))
      (url-set-filename result localname)
      result)))

;; If the gateway runs HTTP 1.0, url-http closes the connection.
;; Therefore we fake it to "1.1".
(defadvice url-http-parse-response
  (after tramp-advice-url-http-parse-response)
  "Set `tramp-gw-current-*' variables, and mark server as HTTP/1.1 compliant."
  (declare (special url-http-process url-http-response-version))
  ;; Massage HTTP process.
  (setq tramp-gw-current-http-proc url-http-process)
  (setq tramp-gw-current-http-buffer (current-buffer))
  ;; We evaluate it in order to avoid byte-compiler warnings.
  (eval (setq url-http-response-version "1.1")))

;; In the case of proxies, url-http calls `url-recreate-url'.  This
;; doesn't handle our filename component, which is in fact the target
;; host:port.  So we must bypass this.
(defadvice url-http-create-request
  (around tramp-advice-url-http-create-request)
  "Reset url-http proxy variables, and trace HTTP call."
  (setq url-http-target-url (tramp-gw-make-urlobj tramp-gw-current-gw-vector)
	url-http-proxy nil)
  ad-do-it
  (let ((string ad-return-value))
    (while (string-match "\\(\r\\|\n\n\\)" string)
      (setq string (replace-match "" nil nil string)))
    (tramp-message tramp-gw-current-vector 6 "\n%s" string))
  ad-return-value)

(defun tramp-gw-http-callback (vec)
  "Set properties for HTTP process.
VEC is the Tramp vector the connection belongs to."
  ;; We are in the HTTP connection buffer.  Make local variables
  ;; visible.
  (declare (special url-http-end-of-headers
		    url-http-process
		    url-http-response-status))
  ;; Deactivate advices, filters and sentinels.
  (ad-deactivate 'url-http-parse-response)
  (ad-deactivate 'url-http-create-request)
  (set-process-sentinel url-http-process nil)
  (set-process-filter url-http-process nil)
  ;; Check response code.
  (if (not (= url-http-response-status 200))
      (progn
	(delete-process url-http-process)
	(tramp-set-connection-property url-http-process "callback" 'error))
    ;; Hide HTTP headers.
    (goto-char url-http-end-of-headers)
    (tramp-message vec 6 "\n%s" (buffer-substring (point-min) (point)))
    (forward-line)
    (narrow-to-region (point) (point-max))
    ;; Massage HTTP process.
    (set-process-buffer url-http-process (current-buffer))
    (tramp-set-connection-property url-http-process "callback" 'ok)
    (tramp-set-process-query-on-exit-flag url-http-process nil)))

(defun tramp-gw-aux-proc-sentinel (proc event)
  "Activate the different filters for involved HTTP and auxiliary processes."
  (when (memq (process-status proc) '(run open))
    ;; A new process has been spawned from `tramp-gw-aux-proc'.
    (tramp-message
     tramp-gw-current-vector 4
     "Opening auxiliary process `%s', speaking with HTTP process `%s'"
     proc tramp-gw-current-http-proc)
    (tramp-set-process-query-on-exit-flag proc nil)
    (tramp-set-connection-property
     tramp-gw-current-http-proc "aux-process" proc)
    (tramp-set-connection-property
     proc "http-process" tramp-gw-current-http-proc)
    ;; Set the process-filter functions for both processes.
    (set-process-filter proc 'tramp-gw-send-to-http-proc-filter)
    (set-process-filter
     tramp-gw-current-http-proc 'tramp-gw-send-to-aux-proc-filter)
    ;; There might be already some output from HTTP process.
    (with-current-buffer (process-buffer tramp-gw-current-http-proc)
      (unless (= (point-min) (point-max))
	(let ((s (buffer-string)))
	  (delete-region (point) (point-max))
	  (tramp-gw-send-to-aux-proc-filter tramp-gw-current-http-proc s))))))

(defun tramp-gw-send-to-http-proc-filter (proc string)
  (process-send-string
   (tramp-get-connection-property proc "http-process" nil) string))

(defun tramp-gw-send-to-aux-proc-filter (proc string)
  (process-send-string
   (tramp-get-connection-property proc "aux-process" nil) string))

(defun tramp-gw-open-connection (vec gw-vec)
  "Open a remote connection to VEC (see `tramp-file-name' structure).
Take GW-VEC as HTTP gateway, i.e. its method must be a gateway method.

It returns a string like \"localhost#port\", which can be used
instead of the host name declared in VEC."

  ;; Remember vectors for property retrieval.
  (setq tramp-gw-current-vector vec
	tramp-gw-current-gw-vector gw-vec)

  (let ((url-debug (or url-debug (>= tramp-verbose 4)))
	(url-package-name "Tramp")
	(url-package-version tramp-version)
	(url-request-method "CONNECT")
	(url-request-data nil)
	(url (tramp-gw-make-urlobj gw-vec)))

    ;; Start listening auxiliary process.
    (unless (and (processp tramp-gw-aux-proc)
		 (memq (process-status tramp-gw-aux-proc) '(listen)))
      (setq tramp-gw-aux-proc
	    (make-network-process
	     :name (buffer-name (tramp-get-buffer gw-vec))
	     :buffer nil :host 'local :server t
	     :noquery t :service t :coding 'raw-text))
      (set-process-sentinel tramp-gw-aux-proc 'tramp-gw-aux-proc-sentinel)
      (tramp-set-process-query-on-exit-flag tramp-gw-aux-proc nil)
      (tramp-message
       vec 4 "Opening auxiliary process `%s', listening on port %d"
       tramp-gw-aux-proc (process-contact tramp-gw-aux-proc :service)))

    ;; Activate advices, in order to receive proper HTTP version, and
    ;; in order to receive traces.
    (ad-activate 'url-http-parse-response)
    (ad-activate 'url-http-create-request)

    ;; Open HTTP connection.
    (url-do-setup)
    (setq tramp-gw-current-http-proc
	  (get-buffer-process
	   (url-http url 'tramp-gw-http-callback (list vec))))
    (tramp-message
     vec 4 "Opening HTTP process `%s'" tramp-gw-current-http-proc)

    ;; Wait for callback.
    (with-timeout (60)
      (while (not (tramp-get-connection-property
		   tramp-gw-current-http-proc "callback" nil))
	(accept-process-output nil 1)))

    ;; Error handling.
    (unless (eq (tramp-get-connection-property
		 tramp-gw-current-http-proc "callback" nil)
		'ok)
      (if (bufferp tramp-gw-current-http-buffer)
	  (with-current-buffer tramp-gw-current-http-buffer
	    (goto-char (point-min))
	    (forward-line)
	    (pop-to-buffer (current-buffer))
	    (tramp-error
	     tramp-gw-current-vector 'file-error "%s"
	     (buffer-substring (point-min) (point))))
	(tramp-error
	 tramp-gw-current-vector 'file-error
	 "Error in opening HTTP connection to %s"
	 (tramp-make-tramp-file-name
	  (tramp-file-name-method gw-vec)
	  (tramp-file-name-user gw-vec)
	  (tramp-file-name-host gw-vec)
	  ""))))

    ;; Return the new host for gateway access.
    (format "localhost#%d" (process-contact tramp-gw-aux-proc :service))))


(provide 'tramp-gw)

;;; TODO:

;; * Provide descriptive Commentary.
;; * Enable it for several gateway processes in parallel.
;; * Implement SOCKS gateway.

;;; arch-tag: fcc9dbec-7503-4d73-b638-3c8aa59575f5
;;; tramp-gw.el ends here
