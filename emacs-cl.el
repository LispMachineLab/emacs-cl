;;; emacs-cl.el --- Common Lisp implemented in Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2003, 2004  Lars Brinkhoff
;; Copyright (C) 2026  Ryan.K

;; Author: Lars Brinkhoff
;; Maintainer: Ryan.K
;; URL: https://github.com/LispMachineLab/emacs-cl
;; Version: 0.6.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: languages, lisp
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

;; Emacs Common Lisp implements Common Lisp in Emacs Lisp; its sources
;; are in cl/.  This file runs it beside the Emacs you work in, in a
;; batch Emacs of its own (see emacs-cl-inferior.el): Common Lisp loads
;; the old cl library and defines hundreds of functions of its own, and
;; a long computation would hold up the editor.
;;
;; `emacs-cl-repl' opens a Common Lisp listener.  `emacs-cl-start'
;; starts a Common Lisp program in a comint buffer, for a mode of one's
;; own, as MYCIN and SHRDLU on LispM Desktop do:
;;
;;   (emacs-cl-start (get-buffer-create "*MYCIN*")
;;                   :load '("auxfns.lisp")
;;                   :eval '("(requires \"mycin\" \"mycin-r\")" "(mycin)"))
;;
;; The first start compiles Common Lisp into `emacs-cl-build-directory',
;; which takes a few seconds; the sources carry no-byte-compile, so that
;; installing the package does not compile them one at a time.  They
;; are in cl/, not src/: package-vc would take src/ for the package's
;; Lisp.

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'lisp-mode)

(defgroup emacs-cl nil
  "Common Lisp implemented in Emacs Lisp."
  :group 'languages
  :prefix "emacs-cl-")

(defconst emacs-cl-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory of this package.")

(defcustom emacs-cl-build-directory (locate-user-emacs-file "emacs-cl/")
  "The directory Common Lisp is compiled into.
It holds a build for each version of the sources and of Emacs; one not
used for a week is deleted."
  :type 'directory)

(defcustom emacs-cl-program (expand-file-name invocation-name invocation-directory)
  "The Emacs that runs Common Lisp, in batch mode."
  :type 'file)

(defun emacs-cl-command (&rest arguments)
  "Return the command running Common Lisp with ARGUMENTS, those of
emacs-cl-inferior.el."
  `(,emacs-cl-program "-Q" "--batch"
    "-l" ,(expand-file-name "emacs-cl-inferior.el" emacs-cl-directory)
    "--source" ,(expand-file-name "cl/" emacs-cl-directory)
    "--build" ,(expand-file-name emacs-cl-build-directory)
    ,@arguments))

(cl-defun emacs-cl-start (buffer &key load eval package repl (name "emacs-cl"))
  "Start Common Lisp in BUFFER, a comint buffer, and return its process.
It loads the files of LOAD, then evaluates the forms of EVAL, strings,
in PACKAGE, a package name, or CL-USER; with REPL non-nil it goes on as a
listener.  NAME names the process.  What Emacs itself says goes to a
buffer of its own, not into BUFFER."
  (let* ((arguments
          (append (mapcan (lambda (file) (list "--load" (expand-file-name file))) load)
                  (and package (list "--package" package))
                  (mapcan (lambda (form) (list "--eval" form)) (ensure-list eval))
                  (and repl (list "--repl"))))
         (stderr (get-buffer-create (format " *%s errors*" name)))
         (process
          (make-process :name name
                        :buffer buffer
                        :command (apply #'emacs-cl-command arguments)
                        ;; A pty would echo what is typed.
                        :connection-type 'pipe
                        :coding 'utf-8-unix
                        :noquery t
                        :stderr stderr)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'comint-mode)
        (comint-mode))
      (set-marker (process-mark process) (point-max))
      (set-process-filter process #'comint-output-filter))
    process))

;;;; Listener

(defvar emacs-cl-repl-buffer-name "*Common Lisp*"
  "The name of the listener's buffer.")

(define-derived-mode emacs-cl-repl-mode comint-mode "Common Lisp"
  "Major mode of the Common Lisp listener, `emacs-cl-repl'.
\\{emacs-cl-repl-mode-map}"
  :syntax-table lisp-mode-syntax-table
  (setq-local comint-prompt-regexp "^[^> \n]+> ")
  (setq-local comint-prompt-read-only t)
  (setq-local comment-start ";")
  (setq-local font-lock-defaults '(lisp-cl-font-lock-keywords-2 nil t))
  (setq-local indent-line-function #'lisp-indent-line))

;;;###autoload
(defun emacs-cl-repl ()
  "Open a Common Lisp listener, starting Common Lisp unless it runs."
  (interactive)
  (let ((buffer (get-buffer-create emacs-cl-repl-buffer-name)))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
        (emacs-cl-repl-mode))
      (emacs-cl-start buffer :repl t :name "common-lisp"))
    (pop-to-buffer-same-window buffer)))

(defun emacs-cl-unload-function ()
  "Stop the listener's Common Lisp; then let unloading go on."
  (let ((buffer (get-buffer emacs-cl-repl-buffer-name)))
    (when-let* ((process (and buffer (get-buffer-process buffer))))
      (delete-process process)))
  nil)

(provide 'emacs-cl)
;;; emacs-cl.el ends here
