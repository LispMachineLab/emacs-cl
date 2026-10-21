;;; emacs-cl-test.el --- Tests of emacs-cl.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K
;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; Starts Common Lisp in a batch Emacs, as `emacs-cl-start' does, with
;; the build in a temporary directory: the first test compiles it.

;;; Code:

(require 'ert)
(require 'emacs-cl)

(defvar emacs-cl-test-build
  (make-temp-file "emacs-cl-test-build-" t)
  "The build directory of the tests.")

(defun emacs-cl-test-wait (process predicate &optional seconds)
  "Wait until PREDICATE returns non-nil in PROCESS's buffer, or SECONDS."
  (let ((end (+ (float-time) (or seconds 120))))
    (while (and (< (float-time) end)
                (not (with-current-buffer (process-buffer process)
                       (funcall predicate))))
      (accept-process-output process 0.1))
    (with-current-buffer (process-buffer process)
      (funcall predicate))))

(defun emacs-cl-test-output-p (regexp)
  "Return a predicate: the buffer holds a match for REGEXP."
  (lambda ()
    (save-excursion
      (goto-char (point-min))
      (re-search-forward regexp nil t))))

(defmacro emacs-cl-test-with-buffer (var &rest body)
  "Run BODY with VAR a new buffer and the tests' build directory."
  (declare (indent 1))
  `(let ((emacs-cl-build-directory emacs-cl-test-build)
         (,var (generate-new-buffer " *emacs-cl test*")))
     (unwind-protect (progn ,@body)
       (when-let* ((process (get-buffer-process ,var)))
         (delete-process process))
       (kill-buffer ,var))))

(ert-deftest emacs-cl-program-output ()
  "A program's output reaches its buffer, and it ends when done."
  (emacs-cl-test-with-buffer buffer
    (let ((process (emacs-cl-start buffer :eval '("(format t \"~R~%\" 42)"))))
      (should (emacs-cl-test-wait process (emacs-cl-test-output-p "forty-two")))
      (should (emacs-cl-test-wait process (lambda () (not (process-live-p process))))))))

(ert-deftest emacs-cl-program-input ()
  "A program reads what is sent to it, and its prompt comes first."
  (emacs-cl-test-with-buffer buffer
    (let ((process (emacs-cl-start
                    buffer :eval '("(progn (format t \"Name? \") (format t \"Hello, ~A.~%\" (read-line)))"))))
      (should (emacs-cl-test-wait process (emacs-cl-test-output-p "Name\\? ")))
      (process-send-string process "Ada\n")
      (should (emacs-cl-test-wait process (emacs-cl-test-output-p "Hello, Ada\\."))))))

(ert-deftest emacs-cl-program-files-and-package ()
  "Files load in order, and forms are read in the package asked for."
  (let ((file (make-temp-file "emacs-cl-test-" nil ".lisp"
                              "(defpackage :greet (:use :cl))\n(in-package :greet)\n(defun hi () :hi)\n")))
    (unwind-protect
        (emacs-cl-test-with-buffer buffer
          (let ((process (emacs-cl-start buffer :load (list file) :package "GREET"
                                         :eval '("(print (hi))"))))
            (should (emacs-cl-test-wait process (emacs-cl-test-output-p ":HI")))))
      (delete-file file))))

(ert-deftest emacs-cl-program-error ()
  "An error the program does not handle is printed and ends it."
  (emacs-cl-test-with-buffer buffer
    (let ((process (emacs-cl-start buffer :eval '("(car 5)" "(print :not-reached)"))))
      (should (emacs-cl-test-wait process (emacs-cl-test-output-p ";; Error: 5 is not of type")))
      (should (emacs-cl-test-wait process (lambda () (not (process-live-p process)))))
      (with-current-buffer buffer
        (should-not (string-search "NOT-REACHED" (buffer-string)))))))

(ert-deftest emacs-cl-listener ()
  "The listener evaluates what is typed, and goes on after an error."
  (emacs-cl-test-with-buffer buffer
    (with-current-buffer buffer (emacs-cl-repl-mode))
    (let ((process (emacs-cl-start buffer :repl t)))
      (should (emacs-cl-test-wait process (emacs-cl-test-output-p "COMMON-LISP-USER> ")))
      (process-send-string process "undefined-variable\n(* 6 7)\n")
      (should (emacs-cl-test-wait process (emacs-cl-test-output-p "^42$")))
      (with-current-buffer buffer
        (should (string-search "The variable UNDEFINED-VARIABLE is unbound" (buffer-string)))))))

(ert-deftest emacs-cl-build-once ()
  "The build is made once and kept while the sources do not change."
  (let ((stamp (expand-file-name "stamp" emacs-cl-test-build)))
    (emacs-cl-test-with-buffer buffer
      (let ((process (emacs-cl-start buffer :eval '("(print 1)"))))
        (should (emacs-cl-test-wait process (lambda () (not (process-live-p process)))))))
    (should (file-exists-p stamp))
    (let ((time (file-attribute-modification-time (file-attributes stamp))))
      (emacs-cl-test-with-buffer buffer
        (let ((process (emacs-cl-start buffer :eval '("(print 2)"))))
          (should (emacs-cl-test-wait process (lambda () (not (process-live-p process)))))
          (with-current-buffer buffer
            (should-not (string-search "Compiling" (buffer-string))))))
      (should (equal time (file-attribute-modification-time (file-attributes stamp)))))))

;;; emacs-cl-test.el ends here
