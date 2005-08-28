;;; tramp-time.el --- Performance tests for Tramp

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes

;; This file is not part of GNU Emacs.

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

;; This package runs performance tests on Tramp. It expects that the
;; Tramp lisp directory is included in the load path.  Preferably,
;; Tramp's Lisp files should be compiled.

;; The test can be adapted by changing `tramp-debug-buffer' and
;; `tramp-verbose', or `tramp-enable-cache', or the test file name, in
;; the `let' clause.  It is expected that the test file should be
;; accessible without password prompting.

;; Three tests are run.  The first one is just performing
;; (file-exists-p test-file) and (file-attributes test-file).  With
;; this test, Tramp's initialization time shall be checked.  Caching
;; file properties should not influence the result.

;; The second and third tests are executing 1000 times (file-exists-p
;; test-file) and (file-attributes test-file), respectively.  This
;; will be heavily influenced by caching the results.  In the
;; non-caching case, Tramp's different implementations of
;; `file-attributes' between Tramp 2.0 and 2.1 might differ.

;; The test can be run with "emacs -l tramp-time.el"

;; Initially, I've got the following results on my Pentium III 700MHz,
;; 256MB RAM, GNU/Debian Linux 2.6.11, Tramp compiled with the
;; respective (X)Emacs version:

;; Tramp       Emacs 20.7  Emacs 21.4  Emacs 22.0  XEmacs 21.4  XEmacs 21.5
;;
;; 2.0.50        13.0 sec    13.0 sec     3.1 sec      3.0 sec      (crash)
;;                4.0 sec     4.0 sec    10.0 sec      5.0 sec      (crash)
;;               22.0 sec    23.0 sec    39.0 sec     25.0 sec      (crash)

;;
;; 2.1.4                -     1.0 sec     0.8 sec        (n/a)      (crash)
;; uncached             -     4.0 sec    10.3 sec        (n/a)      (crash)
;;                      -    14.0 sec    26.5 sec        (n/a)      (crash)
;;
;; 2.1.4                -     1.0 sec     0.8 sec        (n/a)      (crash)
;; cached               -     1.0 sec     1.7 sec        (n/a)      (crash)
;;                      -     2.0 sec     1.7 sec        (n/a)      (crash)

;; Note that Tramp 2.1.4 is applicable for (X)Emacs 21 upwards only.
;; Debian Emacs 20, Emacs 21 and XEmacs 21 (what I use) do not provide
;; milliseconds.  Debian XEmacs 21.4 runs uncompiled Tramp 2.1.4;
;; figures are not included because they are not comparable.  It
;; crashes with compiled Tramp 2.1.4.  XEmacs 21.5 (from CVS) crashes
;; with both compiled and uncompiled Tramp 2.1.4 - no idea why.

;;; Code:

(require 'time-stamp)
(require 'tramp)

(let ((tramp-debug-buffer nil) (tramp-verbose 0)
      (tramp-enable-cache t)
      (test-file (if (featurep 'xemacs) "/[ssh/localhost]/" "/ssh:localhost:/"))
      start-time stop-time)

  ;; Cleanup Tramp buffers.
  (mapcar '(lambda (b)
	     (when (string-match "\\*\\(debug \\)?tramp/" (buffer-name b))
	       (kill-buffer b)))
	  (buffer-list))

  ;; Initialise Result buffer.
  (switch-to-buffer "*result*")
  (erase-buffer)
  (insert
   (format
    "Test accessing \"%s\", emacs-version %s, tramp-version %s, debug level %d, %s cache,%s compiled version\n"
    test-file emacs-version tramp-version tramp-verbose
    (if (and (functionp 'tramp-cache-setup) tramp-enable-cache)
	"with" "without")
    (if (byte-code-function-p (symbol-function 'tramp-message))	"" " not")))
  (sit-for 1)

  ;; First test.  This includes setting up the connection.
  (insert "Start initial connection\n")
  (setq start-time (current-time))
  (file-exists-p test-file)
  (file-attributes test-file)
  (setq stop-time (current-time))
  (insert (format "Stop  initial connection %s sec\n"
		  (tramp-time-diff stop-time start-time)))
  (sit-for 1)

  ;; Second test.  `file-exists-p' just runs "-e test-file" if not cached.
  (insert (format "Start 1000x (file-exists-p \"%s\")\n" test-file))
  (setq start-time (current-time))
  (dotimes (i 1000)
    (file-exists-p test-file))
  (setq stop-time (current-time))
  (insert (format "Stop  1000x (file-exists-p \"%s\") %s sec\n"
		  test-file (tramp-time-diff stop-time start-time)))
  (sit-for 1)

  ;; Third test.  `file-attributes' might run a perl script if not cached.
  (insert (format "Start 1000x (file-attributes \"%s\")\n" test-file))
  (setq start-time (current-time))
  (dotimes (i 1000)
    (file-attributes test-file))
  (setq stop-time (current-time))
  (insert (format "Stop  1000x (file-attributes \"%s\") %s sec\n"
		  test-file (tramp-time-diff stop-time start-time)))
  (sit-for 1))

;;; TODO:

;; * Make it running under test-harness.el.
