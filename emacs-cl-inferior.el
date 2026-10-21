;;; emacs-cl-inferior.el --- Common Lisp in a batch Emacs, over stdin and stdout  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/emacs-cl
;; SPDX-License-Identifier: GPL-2.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `emacs-cl-start' runs this file in a batch Emacs:
;;
;;   emacs -Q --batch -l emacs-cl-inferior.el --source DIR --build DIR
;;         [--load FILE]... [--package NAME] [--eval FORM]... [--repl]
;;
;; It compiles the Common Lisp of the source directory into the build
;; directory unless that was done already, loads it, connects
;; *STANDARD-INPUT* to stdin and *STANDARD-OUTPUT* to stdout, then loads
;; the files, makes NAME the current package and evaluates the forms,
;; in the order given; CL-USER is current until --package.  With --repl
;; it goes on as a listener.  An
;; error that Common Lisp does not handle is printed; it ends the
;; program, or, in the listener, the form.  With --build-only it only
;; compiles.
;;
;; Output is sent at the end of each line and before input is read:
;; batch Emacs holds back what `princ' writes to a pipe.

;;; Code:

(require 'cl-lib)

(defvar emacs-cl-inferior--pending nil
  "Output not sent yet, newest first.")

(defvar emacs-cl-inferior--line ""
  "The line of input being read.")

(defvar emacs-cl-inferior--index 0
  "The index in `emacs-cl-inferior--line' of the next character.")

(defvar emacs-cl-inferior--eof nil
  "Non-nil once stdin is at its end.")

(defvar emacs-cl-inferior--output nil
  "The stream writing stdout.")

(defvar emacs-cl-inferior--at-line-start nil
  "A function telling a stream it is at the start of a line.")

(defun emacs-cl-inferior--say (string)
  "Send STRING to stdout now."
  (send-string-to-terminal string))

;;;; Building

(defun emacs-cl-inferior--sources (source)
  "Return the Lisp files of SOURCE."
  (directory-files source t "\\.el\\'"))

(defun emacs-cl-inferior--stamp (source)
  "Return what a build of SOURCE is made from: Emacs's version and the
names, sizes and times of the sources."
  (prin1-to-string
   (cons emacs-version
         (mapcar (lambda (file)
                   (let ((attributes (file-attributes file)))
                     (list (file-name-nondirectory file)
                           (file-attribute-size attributes)
                           (file-attribute-modification-time attributes))))
                 (emacs-cl-inferior--sources source)))))

(defun emacs-cl-inferior--read-file (file)
  "Return the contents of FILE, or nil when it is not there."
  (and (file-readable-p file)
       (with-temp-buffer
         (insert-file-contents file)
         (buffer-string))))

(defun emacs-cl-inferior--copy (file directory)
  "Copy FILE into DIRECTORY without its no-byte-compile cookie.
The sources have one, so that installing the package does not compile
them one at a time without the rest of Common Lisp loaded."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (goto-char (point-min))
    (when (re-search-forward "; *no-byte-compile: *t" (line-end-position) t)
      (replace-match ""))
    (let ((coding-system-for-write 'no-conversion))
      (write-region nil nil (expand-file-name (file-name-nondirectory file) directory)
                    nil 'silent))))

(defun emacs-cl-inferior-build (source build)
  "Compile the Common Lisp of SOURCE into BUILD, unless it is there already.
The build is made in a directory beside BUILD and then put in its place,
so that a program starting meanwhile finds either the old build or the
new one."
  (let ((stamp (emacs-cl-inferior--stamp source))
        (stamp-file (expand-file-name "stamp" build)))
    (unless (equal stamp (emacs-cl-inferior--read-file stamp-file))
      (emacs-cl-inferior--say "Compiling Common Lisp; it is done once.\n")
      (let* ((parent (file-name-directory (directory-file-name build)))
             (temporary (progn (make-directory parent t)
                               (make-temp-file (expand-file-name "emacs-cl-build-" parent) t))))
        (dolist (file (emacs-cl-inferior--sources source))
          (emacs-cl-inferior--copy file temporary))
        (let ((default-directory (file-name-as-directory temporary)))
          (unless (eq 0 (call-process (expand-file-name invocation-name invocation-directory)
                                      nil nil nil "-Q" "--batch"
                                      "--eval" "(setq inhibit-message t)"
                                      "-l" "load-cl.el" "-f" "compile-cl"))
            (delete-directory temporary t)
            (error "Compiling Common Lisp failed")))
        (with-temp-file (expand-file-name "stamp" temporary)
          (insert stamp))
        (when (file-directory-p build)
          (let ((old (make-temp-name (expand-file-name "emacs-cl-old-" parent))))
            (rename-file (directory-file-name build) old)
            (delete-directory old t)))
        (condition-case nil
            (rename-file temporary (directory-file-name build))
          ;; Another program put its build there first.
          (file-already-exists (delete-directory temporary t)))))))

;;;; Streams

(defun emacs-cl-inferior--flush ()
  "Send the output written so far."
  (when emacs-cl-inferior--pending
    (emacs-cl-inferior--say (apply #'concat (nreverse emacs-cl-inferior--pending)))
    (setq emacs-cl-inferior--pending nil)))

(defun emacs-cl-inferior--write (char _stream)
  "Write CHAR, a Common Lisp character, to stdout."
  (let ((code (CHAR-CODE char)))
    (push (char-to-string code) emacs-cl-inferior--pending)
    (when (eq code ?\n)
      (emacs-cl-inferior--flush))))

(defun emacs-cl-inferior--read (_stream)
  "Return the next character of stdin, or :eof at its end."
  (emacs-cl-inferior--flush)
  (when (and (not emacs-cl-inferior--eof)
             (>= emacs-cl-inferior--index (length emacs-cl-inferior--line)))
    (setq emacs-cl-inferior--line
          ;; Common Lisp's EVAL turns every signal into a Common Lisp
          ;; error, even inside `condition-case'.
          (let ((debug-on-signal nil)
                (debug-on-error nil))
            (condition-case nil
                (concat (read-from-minibuffer "") "\n")
              (error (setq emacs-cl-inferior--eof t) "")))
          emacs-cl-inferior--index 0))
  (if emacs-cl-inferior--eof
      :eof
    (let ((code (aref emacs-cl-inferior--line emacs-cl-inferior--index)))
      (setq emacs-cl-inferior--index (1+ emacs-cl-inferior--index))
      ;; The newline ending a line typed leaves the terminal at the
      ;; start of a line, so FRESH-LINE has nothing to do.
      (when (and (eq code ?\n) emacs-cl-inferior--output)
        (funcall emacs-cl-inferior--at-line-start emacs-cl-inferior--output))
      (CODE-CHAR code))))

(defun emacs-cl-inferior--connect ()
  "Make the standard streams read stdin and write stdout."
  (let* ((in (MAKE-STREAM (keyword "direction") (keyword "INPUT")
                          (keyword "read-fn") #'emacs-cl-inferior--read
                          (keyword "position") nil))
         (out (MAKE-STREAM (keyword "direction") (keyword "OUTPUT")
                           (keyword "write-fn") #'emacs-cl-inferior--write))
         (io (MAKE-TWO-WAY-STREAM in out)))
    (setq emacs-cl-inferior--output out
          ;; Made now: STREAM's accessors exist only once Common Lisp is
          ;; loaded.
          emacs-cl-inferior--at-line-start
          (eval '(lambda (stream) (setf (STREAM-fresh-line-p stream) t)) t)
          *STANDARD-INPUT* in
          *STANDARD-OUTPUT* out
          *ERROR-OUTPUT* out
          *TRACE-OUTPUT* out
          *TERMINAL-IO* io
          *QUERY-IO* io
          *DEBUG-IO* io)))

;;;; Errors

(defun emacs-cl-inferior--describe (condition)
  "Return what CONDITION says, as a string."
  (let ((debug-on-signal nil)
        (debug-on-error nil))
    (emacs-cl-inferior--describe-1 condition)))

(defun emacs-cl-inferior--describe-1 (condition)
  "Return what CONDITION says, as a string."
  (or (ignore-errors
        ;; Simple errors, warnings and type errors have a control string
        ;; too, though not of type SIMPLE-CONDITION here.
        (let ((control (SIMPLE-CONDITION-FORMAT-CONTROL condition)))
          (and control
               (apply #'FORMAT nil control
                      (SIMPLE-CONDITION-FORMAT-ARGUMENTS condition)))))
      (ignore-errors
        (and (TYPEP condition 'TYPE-ERROR)
             (FORMAT nil "~S is not of type ~S" (TYPE-ERROR-DATUM condition)
                     (TYPE-ERROR-EXPECTED-TYPE condition))))
      (ignore-errors
        (and (TYPEP condition 'UNBOUND-VARIABLE)
             (FORMAT nil "The variable ~S is unbound" (CELL-ERROR-NAME condition))))
      (ignore-errors
        (and (TYPEP condition 'UNDEFINED-FUNCTION)
             (FORMAT nil "The function ~S is undefined" (CELL-ERROR-NAME condition))))
      (ignore-errors (FORMAT nil "~A" condition))
      (format "%S" condition)))

(defun emacs-cl-inferior--debugger (condition)
  "Take the place of INVOKE-DEBUGGER, which would read commands from the
terminal: call *DEBUGGER-HOOK*, then give up the form being evaluated."
  (let ((hook *DEBUGGER-HOOK*))
    (when hook
      (let ((*DEBUGGER-HOOK* nil))
        (FUNCALL hook condition hook))))
  (throw 'emacs-cl-inferior--error
         (list :error (emacs-cl-inferior--describe condition))))

(defun emacs-cl-inferior--protect (function)
  "Call FUNCTION; print an error Common Lisp does not handle, and return
:error then."
  (let ((result
         (catch 'emacs-cl-inferior--error
           (condition-case error
               (list :value (funcall function))
             (error (list :error (error-message-string error)))))))
    (if (eq (car result) :error)
        (progn
          (emacs-cl-inferior--flush)
          (emacs-cl-inferior--say (format "\n;; Error: %s\n" (cadr result)))
          :error)
      (cadr result))))

;;;; Listener

(defun emacs-cl-inferior--repl ()
  "Read forms from stdin, evaluate them and print their values, until
stdin ends."
  (let ((eof (make-symbol "eof")))
    (catch 'emacs-cl-inferior--done
      (while t
        (FORMAT T "~&~A> " (PACKAGE-NAME *PACKAGE*))
        (emacs-cl-inferior--protect
         (lambda ()
           (let ((form (READ *STANDARD-INPUT* nil eof)))
             (when (eq form eof)
               (throw 'emacs-cl-inferior--done nil))
             (dolist (value (emacs-cl-eval-interactively form))
               (FORMAT T "~&~S~%" value)))))))))

;;;; Main

(defun emacs-cl-inferior-main ()
  "Do what the command line asks, as the Commentary says."
  (let (source build actions repl build-only)
    (while command-line-args-left
      (let ((argument (pop command-line-args-left)))
        (pcase argument
          ("--source" (setq source (pop command-line-args-left)))
          ("--build" (setq build (pop command-line-args-left)))
          ("--package" (push (cons 'package (pop command-line-args-left)) actions))
          ("--load" (push (cons 'load (pop command-line-args-left)) actions))
          ("--eval" (push (cons 'eval (pop command-line-args-left)) actions))
          ("--repl" (setq repl t))
          ("--build-only" (setq build-only t))
          (_ (error "Unknown argument %s" argument)))))
    (unless (and source build)
      (error "--source and --build are required"))
    (setq source (file-name-as-directory (expand-file-name source))
          build (file-name-as-directory (expand-file-name build)))
    (emacs-cl-inferior-build source build)
    (unless build-only
      (let ((load-path (cons build load-path))
            (load (symbol-function 'load)))
        ;; load-cl.el loads each file with a message.
        (cl-letf (((symbol-function 'load)
                   (lambda (file &optional noerror _nomessage nosuffix must-suffix)
                     (funcall load file noerror t nosuffix must-suffix))))
          (load (expand-file-name "load-cl.el" build) nil t)))
      (emacs-cl-inferior--connect)
      (defalias 'INVOKE-DEBUGGER #'emacs-cl-inferior--debugger)
      (catch 'emacs-cl-inferior--stop
        (dolist (action (nreverse actions))
          (when (eq :error
                    (emacs-cl-inferior--protect
                     (lambda ()
                       (pcase action
                         (`(load . ,file) (LOAD file))
                         (`(package . ,name)
                          (setq *PACKAGE* (or (FIND-PACKAGE name)
                                              (ERROR "There is no package ~A" name))))
                         (`(eval . ,form) (EVAL (READ-FROM-STRING form)))))))
            (unless repl
              (throw 'emacs-cl-inferior--stop nil)))))
      (when repl
        (emacs-cl-inferior--repl))
      (emacs-cl-inferior--flush))))

(when noninteractive
  ;; Emacs's own messages, as about the old cl library Common Lisp
  ;; loads, are not the program's output.
  (setq inhibit-message t)
  (emacs-cl-inferior-main)
  (kill-emacs 0))

;;; emacs-cl-inferior.el ends here
