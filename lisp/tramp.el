;;; rssh.el --- remote file editing using ssh/scp

;; Copyright (C) 1998 by Kai Grossjohann

;; Author: Kai.Grossjohann@CS.Uni-Dortmund.DE
;; Keywords: comm, processes
;; Version: $Id: tramp.el,v 1.2 1998/11/30 18:07:48 grossjoh Exp $

;; rssh.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; rssh.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides remote file editing, similar to ange-ftp.
;; The difference is that ange-ftp uses FTP to transfer files between
;; the local and the remote host, whereas rssh.el uses a combination
;; of ssh and scp.
;;
;; Installation is simple -- it is sufficient to load this file.
;;
;; Usage is also simple: it's just like ange-ftp, but uses a different
;; syntax for the remote file names.  The syntax used is as follows:
;;
;; /s:USER@HOST:FILENAME
;;
;; This logs you in as USER to the remote HOST, retrieving FILENAME.
;;
;; WARNING!
;;
;; This file is in a very early stage of development, there are a
;; number of limitations and of course a lot of unknown bugs.
;;
;; Firstly, you MUST have set up ssh such that you can log in as USER
;; to the remote HOST without entering a password or pass phrase.  The
;; code does *not* check if you have done this, so expect strange
;; effects when ssh asks you for a password or pass phrase.  (It is
;; fairly easy to allow login without entering a password or pass
;; phrase by using ssh-agent, or by using the .shosts file, or even
;; the .rhosts file.  Thus, I did not deem it important to deal with
;; querying for passwords in this early stage of development.)
;;
;; Secondly, there is almost no error checking at all.
;;
;; I have thus far tested this on one machine, as one user, with two
;; files and one directory.
;;
;; Here's what seems to work so far:
;;   - typing C-x C-f then entering a complete absolute file name
;;   - filename completion (for absolute file names)
;;   - saving files
;;   - getting a directory listing with dired (But why does `f' not work?)


;;; TODO:

;; diff-latest-backup-file -- in diff.el
;; directory-file-name -- use primitive?
;; dired-compress-file
;; dired-uncache
;; file-modes
;; file-name-as-directory -- use primitive?
;; file-name-completion
;; file-name-directory -- use primitive?
;; file-name-nondirectory -- use primitive?
;; file-name-sans-versions -- use primitive?
;; file-newer-than-file-p
;; file-ownership-preserved-p
;; file-truename
;; find-backup-file-name
;; get-file-buffer
;; load
;; make-directory
;; make-symbolic-link
;; rename-file
;; set-file-modes
;; set-visited-file-modtime
;; shell-command
;; unhandled-file-name-directory
;; vc-registered
;; verify-visited-file-modtime

;;; Code:

(provide 'rssh)

;;; User Customizable Internal Variables:

(defvar rssh-ssh-program "ssh"
  "Name of ssh program.")

(defvar rssh-ssh-end-of-line "\n"
  "String used for end of line in ssh connections.")

;;; Internal Variables:

(defvar rssh-buffer-process nil
  "The process of the current buffer.  This variable is buffer-local.")

;;; File Name Handler Functions:

(defun rssh-handle-file-exists-p (filename)
  "Like `file-exists-p' for rssh files."
  (let ((v (rssh-dissect-file-name filename))
        user host path)
    (setq user (aref v 0))
    (setq host (aref v 1))
    (setq path (aref v 2))
    (save-excursion
      (rssh-send-command user host
                         (format "ls -d '%s' 2>&1 > /dev/null ; echo $?" path))
      (rssh-wait-for-output)
      (goto-char (point-min))
      (zerop (read (current-buffer))))))

(defun rssh-handle-file-directory-p (filename)
  (eq t (car (rssh-handle-file-attributes filename))))

;; Simple functions implemented with `test'.
(defun rssh-handle-file-executable-p (filename)
  "Like `file-executable-p' for rssh files."
  (zerop (rssh-run-test "-x" filename)))

;; What is a directory name spec?  We need to grok that, too.
(defun rssh-handle-file-accessible-directory-p (filename)
  "Like `file-accessible-directory-p' for rssh files."
  (and (zerop (rssh-run-test "-d" filename))
       (zerop (rssh-run-test "-r" filename))
       (zerop (rssh-run-test "-x" filename))))

(defun rssh-handle-file-readable-p (filename)
  "Like `file-readable-p' for rssh files."
  (zerop (rssh-run-test "-r" filename)))

(defun rssh-handle-file-regular-p (filename)
  "Like `file-regular-p' for rssh files."
  (eq ?- (aref (nth 8 (rssh-handle-file-attributes filename)) 0)))

(defun rssh-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for rssh files."
  (stringp (car (rssh-handle-file-attributes filename))))

(defun rssh-handle-file-writable-p (filename)
  "Like `file-writable-p' for rssh files."
  (if (rssh-handle-file-exists-p filename)
      ;; Existing files must be writable.
      (zerop (rssh-run-test "-w" filename))
    ;; If file doesn't exist, check if directory is writable.
    (and (zerop (rssh-run-test "-d" (file-name-nondirectory filename)))
         (zerop (rssh-run-test "-w" (file-name-nondirectory filename))))))

;; Other file attributes.

(defun rssh-handle-file-attributes (filename)
  "Like `file-attributes' for rssh files."
  (let ((v (rssh-dissect-file-name filename))
        user host path symlinkp dirp
        res-inode res-filemodes res-numlinks
        res-uid res-gid res-size res-symlink-target)
    (setq user (aref v 0))
    (setq host (aref v 1))
    (setq path (aref v 2))
    (if (not (rssh-handle-file-exists-p filename))
        nil                             ; file cannot be opened
      ;; file exists, find out stuff
      (save-excursion
        (rssh-send-command user host (format "ls -iLldn %s" path))
        (rssh-wait-for-output)
        (goto-char (point-min))
        ;; parse `ls -l' output ...
        ;; ... inode
        (setq res-inode (read (current-buffer)))
        ;; ... file mode flags
        (setq res-filemodes (symbol-name (read (current-buffer))))
        ;; ... number links
        (setq res-numlinks (read (current-buffer)))
        ;; ... uid and gid
        (setq res-uid (read (current-buffer)))
        (setq res-gid (read (current-buffer)))
        ;; ... size
        (setq res-size (read (current-buffer)))
        ;; From the file modes, figure out other stuff.
        (setq symlinkp (eq ?l (aref res-filemodes 0)))
        (setq dirp (eq ?d (aref res-filemodes 0)))
        ;; if symlink, find out file name pointed to
        (when symlinkp
          (search-forward "-> ")
          (setq res-symlink-target
                (buffer-substring (point)
                                  (progn (end-of-line) (point))))))
      ;; return data gathered
      (list
       ;; 0. t for directory, string (name linked to) for symbolic
       ;; link, or nil.
       (or dirp res-symlink-target nil)
       ;; 1. Number of links to file.
       res-numlinks
       ;; 2. File uid.
       res-uid
       ;; 3. File gid.
       res-gid
       ;; 4. Last access time, as a list of two integers. First
       ;; integer has high-order 16 bits of time, second has low 16
       ;; bits.
       ;; 5. Last modification time, likewise.
       ;; 6. Last status change time, likewise.
       '(0 0) '(0 0) '(0 0)             ;CCC how to find out?
       ;; 7. Size in bytes (-1, if number is out of range).
       res-size
       ;; 8. File modes, as a string of ten letters or dashes as in ls -l.
       res-filemodes
       ;; 9. t iff file's gid would change if file were deleted and
       ;; recreated.
       nil                              ;hm?
       ;; 10. inode number.
       res-inode
       ;; 11. Device number.
       -1                               ;hm?
       ))))
       
;; Other file name ops.

(defun rssh-handle-file-truename (filename &optional counter prev-dirs)
  "Like `file-truename' for rssh files."
  filename)                             ;CCC what to do?
    

;; Directory listings.
(defun rssh-handle-directory-files (directory &optional full match nosort)
  "Like `directory-files' for rssh files."
  (let ((v (rssh-dissect-file-name directory))
        user host path result x)
    (setq user (aref v 0))
    (setq host (aref v 1))
    (setq path (aref v 2))
    (save-excursion
      (if full
          (rssh-send-command user host
                             (format "ls -ad %s" path))
        (rssh-send-command user host (format "cd %s" path))
        (rssh-send-command user host "ls -a"))
      (rssh-wait-for-output)
      (goto-char (point-max))
      (while (zerop (forward-line -1))
        (setq x (buffer-substring (point)
                                  (progn (end-of-line) (point))))
        (if match
            (when (string-match match x) (push x result))
          (push x result))))
    result))

(defun rssh-handle-file-name-all-completions (file directory)
  "Like `file-name-all-completions' for rssh files."
  (let ((v (rssh-dissect-file-name directory))
        user host path result)
    (setq user (aref v 0))
    (setq host (aref v 1))
    (setq path (aref v 2))
    (save-excursion
      (rssh-send-command user host (format "cd %s" path))
      (rssh-send-command user host
                         (format "ls -adF %s* 2>/dev/null" file))
      (rssh-wait-for-output)
      (goto-char (point-max))
      (while (zerop (forward-line -1))
        (push (buffer-substring (point)
                                (progn (end-of-line) (point)))
              result)))
    result))

;; cp, mv and ln
(defun rssh-handle-add-name-to-file
  (file newname &optional ok-if-already-exists)
  "Like `add-name-to-file' for rssh files."
  (error "add-name-to-file not implemented yet for rssh files."))

(defun rssh-handle-copy-file
  (file newname &optional ok-if-already-exists)
  "Like `copy-file' for rssh files."
  (error "copy-file not implemented yet for rssh files."))

;; error checking?
(defun rssh-handle-delete-directory (directory)
  "Like `delete-directory' for rssh files."
  (let ((v (rssh-dissect-file-name directory))
        user host path result)
    (setq user (aref v 0))
    (setq host (aref v 1))
    (setq path (aref v 2))
    (save-excursion
      (rssh-send-command user host (format "rmdir %s ; echo ok" path))
      (rssh-wait-for-output))))

(defun rssh-handle-delete-file (filename)
  "Like `delete-file' for rssh files."
  (let ((v (rssh-dissect-file-name filename))
        user host path result)
    (setq user (aref v 0))
    (setq host (aref v 1))
    (setq path (aref v 2))
    (save-excursion
      (rssh-send-command user host (format "rm -f %s ; echo ok" path))
      (rssh-wait-for-output))))

;; Dired.

(defun rssh-handle-dired-call-process (program discard &rest arguments)
  "Like `dired-call-process' for rssh files."
  (let ((v (rssh-dissect-file-name default-directory))
        user host path result)
    (setq user (aref v 0))
    (setq host (aref v 1))
    (setq path (aref v 2))
    (save-excursion
      (rssh-send-command user host path)
      (rssh-send-command user host
                         (mapconcat #'identity (cons program arguments)))
      (rssh-wait-for-output))
    (unless discard
      (insert-buffer (rssh-get-buffer user host)))
    (save-excursion
      (rssh-send-command user host "echo $?")
      (rssh-wait-for-output)
      (read (rssh-get-buffer user host)))))

(defun rssh-handle-insert-directory
  (file switches &optional wildcard full-directory-p)
  "Like `insert-directory' for rssh files."
  (let ((v (rssh-dissect-file-name file))
        user host path)
    (setq user (aref v 0))
    (setq host (aref v 1))
    (setq path (aref v 2))
    (when (listp switches)
      (setq switches (mapconcat #'identity switches " ")))
    (unless full-directory-p
      (setq switches (concat "-d " switches)))
    (save-excursion
      (rssh-send-command user host
                         (format "ls %s %s" switches path))
      (rssh-wait-for-output))
    (insert-buffer (rssh-get-buffer user host))))

;; Canonicalization of file names.

(defun rssh-handle-expand-file-name (name &optional default-directory)
  "Like `expand-file-name' for rssh files."
  name)

;; File Editing.

(defun rssh-handle-file-local-copy (file)
  "Like `file-local-copy' for rssh files."
  (let ((v (rssh-dissect-file-name filename))
        user host path tmpfil)
    (setq user (aref v 0))
    (setq host (aref v 1))
    (setq path (aref v 2))
    (setq tmpfil (make-temp-name "/tmp/rssh."))
    (call-process "scp" nil nil nil
                  (format "%s@%s:%s" user host path)
                  tmpfil)
    tmpfil))

;; CCC need to do MULE stuff
(defun rssh-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for rssh files."
  (let ((local-copy (rssh-handle-file-local-copy filename)))
    (when visit
      (setq buffer-file-name filename)
      (set-visited-file-modtime '(0 0))
      (set-buffer-modified-p nil))
    (rssh-run-real-handler 'insert-file-contents
                           (list local-copy nil beg end replace))
    (delete-file local-copy)
    ))

;; CCC need to do MULE stuff
(defun rssh-handle-write-region
  (start end filename &optional append visit lockname confirm)
  "Like `write-region' for rssh files."
  (unless (eq visit t)
    (error "rssh-handle-write-region: VISIT must be t."))
  (unless (eq append nil)
    (error "rssh-handle-write-region: APPEND must be nil."))
  (unless (eq lockname nil)
    (error "rssh-handle-write-region: LOCKNAME must be nil."))
  (unless (eq confirm nil)
    (error "rssh-handle-write-region; CONFIRM must be nil."))
  (let ((v (rssh-dissect-file-name filename))
        user host path tmpfil)
    (setq user (aref v 0))
    (setq host (aref v 1))
    (setq path (aref v 2))
    (setq tmpfil (make-temp-name "/tmp/rssh."))
    (rssh-run-real-handler
     'write-region
     (list start end tmpfil append 'no-message lockname confirm))
    (call-process "scp" nil nil nil
                  tmpfil
                  (format "%s@%s:%s" user host path))
    (delete-file tmpfil)
    (message "Wrote %s" filename)))

;; Main function.
(defun rssh-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers
	 (cons 'rssh-file-name-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

(defun rssh-file-name-handler (operation &rest args)
  (cond ((eq operation 'file-exists-p)
         (apply #'rssh-handle-file-exists-p args))
        ((eq operation 'file-directory-p)
         (apply #'rssh-handle-file-directory-p args))
        ((eq operation 'file-executable-p)
         (apply #'rssh-handle-file-executable-p args))
        ((eq operation 'file-accessible-directory-p)
         (apply #'rssh-handle-file-accessible-directory-p args))
        ((eq operation 'file-readable-p)
         (apply #'rssh-handle-file-readable-p args))
        ((eq operation 'file-regular-p)
         (apply #'rssh-handle-file-regular-p args))
        ((eq operation 'file-symlink-p)
         (apply #'rssh-handle-file-symlink-p args))
        ((eq operation 'file-writable-p)
         (apply #'rssh-handle-file-writable-p args))
        ((eq operation 'file-attributes)
         (apply #'rssh-handle-file-attributes args))
        ((eq operation 'file-directory-files)
         (apply #'rssh-handle-file-directory-files args))
        ((eq operation 'file-name-all-completions)
         (apply #'rssh-handle-file-name-all-completions args))
        ((eq operation 'add-name-to-file)
         (apply #'rssh-handle-add-name-to-file args))
        ((eq operation 'copy-file)
         (apply #'rssh-handle-copy-file args))
        ((eq operation 'delete-directory)
         (apply #'rssh-handle-delete-directory args))
        ((eq operation 'delete-file)
         (apply #'rssh-handle-delete-file args))
        ((eq operation 'dired-call-process)
         (apply #'rssh-handle-dired-call-process args))
        ((eq operation 'insert-directory)
         (apply #'rssh-handle-insert-directory args))
        ((eq operation 'expand-file-name)
         (apply #'rssh-handle-expand-file-name args))
        ((eq operation 'file-local-copy)
         (apply #'rssh-handle-file-local-copy args))
        ((eq operation 'insert-file-contents)
         (apply #'rssh-handle-insert-file-contents args))
        ((eq operation 'write-region)
         (apply #'rssh-handle-write-region args))
        ;; handle ops we don't know about
        (t (rssh-run-real-handler operation args))))

;; Register in file name handler alist

(add-to-list 'file-name-handler-alist
             '("\\`/s:" . rssh-file-name-handler))

;;; Internal Functions:

(defun rssh-run-test (switch filename)
  "Run `test' on the remote system, given a switch and a file.
Returns the exit code of test."
  (let ((v (rssh-dissect-file-name filename))
        user host path result)
    (setq user (aref v 0))
    (setq host (aref v 1))
    (setq path (aref v 2))
    (save-excursion
      (rssh-send-command user host
                         (format "test %s \"%s\" ; echo $?" switch path))
      (rssh-wait-for-output)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun rssh-buffer-name (user host)
  "A name for the connection buffer for USER at HOST."
  (format "*rssh %s@%s*" user host))

(defun rssh-get-buffer (user host)
  "Get the connection buffer to be used for USER at HOST."
  (get-buffer-create (rssh-buffer-name user host)))

(defun rssh-open-connection-ssh (user host)
  "Open a connection to HOST, logging in as USER, using ssh."
  (set-buffer (rssh-get-buffer user host))
  (erase-buffer)
  (make-local-variable 'rssh-buffer-process)
  (setq rssh-buffer-process
        (start-process (rssh-buffer-name user host)
                       (rssh-get-buffer user host) 
                       rssh-ssh-program
                       "-e" "none"
                       "-l" user host
                       "/bin/sh")))

(defun rssh-maybe-open-connection-ssh (user host)
  "Open a connection to HOST, logging in as USER, using ssh, if none exists."
  (save-excursion
    (set-buffer (rssh-get-buffer user host))
    (unless (and rssh-buffer-process
                 (processp rssh-buffer-process))
      (rssh-open-connection-ssh user host))))

(defun rssh-send-command (user host command)
  "Send the COMMAND to USER at HOST."
  (rssh-maybe-open-connection-ssh user host)
  (let ((proc nil))
    (set-buffer (rssh-get-buffer user host))
    (erase-buffer)
    (setq proc (get-buffer-process (current-buffer)))
    (process-send-string proc
                         (concat command rssh-ssh-end-of-line))))

(defun rssh-wait-for-output ()
  "Wait for output from remote ssh command."
  (accept-process-output rssh-buffer-process))

(defun rssh-rssh-file-p (name)
  "Return t iff this is an rssh file."
  (string-match "\\`/s:" name))

(defun rssh-dissect-file-name (name)
  "Returns a vector: remote user, remote host, remote path name."
  (unless (string-match "\\`/s:\\([a-z]+\\)@\\([a-z.-]+\\):\\(.*\\)\\'"
                        name)
    (error "Not an rssh file name: %s" name))
  (vector
   (match-string 1 name)
   (match-string 2 name)
   (match-string 3 name)))

;;; rssh.el ends here
