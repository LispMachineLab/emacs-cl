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

(add-hook 'kill-emacs-hook
          (lambda () (delete-directory emacs-cl-test-build t)))

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
      (should (emacs-cl-test-wait process (emacs-cl-test-output-p "> 42$")))
      (with-current-buffer buffer
        (should (string-search "The variable UNDEFINED-VARIABLE is unbound" (buffer-string)))))))

(ert-deftest emacs-cl-build-once ()
  "The build is made once and kept while the sources do not change."
  (emacs-cl-test-with-buffer buffer
    (let ((process (emacs-cl-start buffer :eval '("(print 1)"))))
      (should (emacs-cl-test-wait process (lambda () (not (process-live-p process)))))))
  (let ((stamp (car (directory-files-recursively emacs-cl-test-build "\\`stamp\\'"))))
    (should stamp)
    (let ((time (file-attribute-modification-time (file-attributes stamp))))
      (emacs-cl-test-with-buffer buffer
        (let ((process (emacs-cl-start buffer :eval '("(print 2)"))))
          (should (emacs-cl-test-wait process (lambda () (not (process-live-p process)))))
          (with-current-buffer buffer
            (should-not (string-search "Compiling" (buffer-string))))))
      (should (equal time (file-attribute-modification-time (file-attributes stamp)))))))

(ert-deftest emacs-cl-program-exit-status ()
  "A program ending on an error exits with status 1, one that ends well 0."
  (emacs-cl-test-with-buffer buffer
    (let ((process (emacs-cl-start buffer :eval '("(car 5)"))))
      (should (emacs-cl-test-wait process (lambda () (not (process-live-p process)))))
      (should (eq 1 (process-exit-status process)))))
  (emacs-cl-test-with-buffer buffer
    (let ((process (emacs-cl-start buffer :eval '("(+ 1 2)"))))
      (should (emacs-cl-test-wait process (lambda () (not (process-live-p process)))))
      (should (eq 0 (process-exit-status process))))))

(ert-deftest emacs-cl-listener-drops-the-line-after-an-error ()
  "After an error the rest of the line is not read as more forms."
  (emacs-cl-test-with-buffer buffer
    (let ((process (emacs-cl-start buffer :repl t)))
      (should (emacs-cl-test-wait process (emacs-cl-test-output-p "COMMON-LISP-USER> ")))
      (process-send-string process "(list nopkg:bar 1 (print :side-effect))\n(+ 20 22)\n")
      (should (emacs-cl-test-wait process (emacs-cl-test-output-p "> 42$")))
      (with-current-buffer buffer
        (should (string-search "NOPKG" (buffer-string)))
        (should-not (string-search "SIDE-EFFECT" (buffer-string)))))))

(ert-deftest emacs-cl-listener-reads-the-next-line ()
  "READ-LINE typed at the listener reads the line after it, not the
newline ending its own."
  (emacs-cl-test-with-buffer buffer
    (let ((process (emacs-cl-start buffer :repl t)))
      (should (emacs-cl-test-wait process (emacs-cl-test-output-p "COMMON-LISP-USER> ")))
      (process-send-string process "(read-line)\nhello there\n")
      (should (emacs-cl-test-wait process (emacs-cl-test-output-p "\"hello there\""))))))

(ert-deftest emacs-cl-common-lisp ()
  "Init forms see outer variables; FORMAT prints floats as CLHS says."
  (emacs-cl-test-with-buffer buffer
    (let ((process (emacs-cl-start
                    buffer :eval '("(print (let ((x 10)) (funcall (lambda (&optional (x x)) x))))"
                                   "(print (list (format nil \"~F\" 1e-5) (format nil \"~,,,2E\" 1234.5) (format nil \"~,0F\" 2.5)))"))))
      (should (emacs-cl-test-wait process (lambda () (not (process-live-p process)))))
      (with-current-buffer buffer
        (should (string-search "\n10 " (buffer-string)))
        (should (string-search "(\"0.00001\" \"12.345e+2\" \"2.\")" (buffer-string)))))))

(ert-deftest emacs-cl-compiling-as-it-runs ()
  "DEFSTRUCT, COMPILE and COMPILE-FILE compile as Common Lisp is built:
with dynamic binding, and on Emacs 30 too, whose warnings.el loads
icons.el."
  (let* ((file (make-temp-file "emacs-cl-test-" nil ".lisp" "(defun twice (n) (* 2 n))\n"))
         (base (file-name-sans-extension file)))
    (unwind-protect
        (emacs-cl-test-with-buffer buffer
          (let ((process (emacs-cl-start
                          buffer :eval `("(defstruct point x y)"
                                         "(print (point-y (make-point :x 1 :y 2)))"
                                         "(defun bar () (declare (special %bar%)) %bar%)"
                                         "(print (funcall (compile nil '(lambda () (let ((%bar% 99)) (declare (special %bar%)) (bar))))))"
                                         ,(format "(load (compile-file %S))" file)
                                         "(print (twice 21))"))))
            (should (emacs-cl-test-wait process (lambda () (not (process-live-p process)))))
            (should (eq 0 (process-exit-status process)))
            (with-current-buffer buffer
              (should (string-match-p "\n2 \\(.\\|\n\\)*\n99 \\(.\\|\n\\)*\n42 " (buffer-string))))))
      (dolist (suffix '(".lisp" ".el" ".elc"))
        (when (file-exists-p (concat base suffix))
          (delete-file (concat base suffix)))))))

;;; emacs-cl-test.el ends here
