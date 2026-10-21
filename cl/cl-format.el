;;;; -*- mode: emacs-lisp; no-byte-compile: t -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements the FORMAT function from chapter 22, Printer.

;;; A control string is parsed once into a list of strings and
;;; directives, and cached.  ~[, ~{, ~( and ~< hold the clauses they
;;; enclose.  Output goes to a temporary buffer, so that ~T, ~&, ~(
;;; and ~< can look at what was written, and the buffer's text is
;;; written to the destination at the end.

(IN-PACKAGE "EMACS-CL")

(cl-defstruct (fmt-dir (:constructor make-fmt-dir (char params colonp atp)))
  char params colonp atp
  name			; function name of ~/name/
  clauses		; ~[ ~{ ~( ~<: list of item lists
  separators		; the ~; directives between the clauses
  close)		; the closing directive

(defvar fmt-cache (make-hash-table :test 'equal))

(defvar fmt-args)	; vector of arguments at this level
(defvar fmt-pos)	; index of the next argument
(defvar fmt-stream)	; destination stream, or nil
(defvar fmt-outer nil)	; arguments left to ~:{ when inside it

(defun fmt-error (control index message)
  (ERROR "~A~%  in FORMAT control ~S at ~D" message control index))

;;; Parsing ----------------------------------------

(defun fmt-digit-p (c)
  (and c (<= ?0 c) (<= c ?9)))

(defun fmt-read-params (control i)
  "Read directive parameters at I.  Return (PARAMS . NEXT-INDEX)."
  (let ((n (length control))
	(params nil)
	(more t))
    (while more
      (let ((c (and (< i n) (aref control i)))
	    (param nil))
	(cond
	  ((or (fmt-digit-p c)
	       (and (memq c '(?+ ?-))
		    (fmt-digit-p (and (< (1+ i) n) (aref control (1+ i))))))
	   (let ((j (1+ i)))
	     (while (fmt-digit-p (and (< j n) (aref control j)))
	       (incf j))
	     (setq param (string-to-number (substring control i j))
		   i j)))
	  ((eq c ?')
	   (when (>= (1+ i) n)
	     (fmt-error control i "Missing character after '"))
	   (setq param (list 'quote (aref control (1+ i)))
		 i (+ i 2)))
	  ((memq c '(?v ?V))
	   (setq param 'V i (1+ i)))
	  ((eq c ?#)
	   (setq param 'N i (1+ i))))
	(push param params)
	(if (and (< i n) (eq (aref control i) ?,))
	    (incf i)
	    (setq more nil))))
    (setq params (nreverse params))
    (cons (if (equal params '(nil)) nil params) i)))

(defun fmt-tokens (control)
  "Split CONTROL into strings and fmt-dir structures."
  (let ((n (length control))
	(i 0)
	(start 0)
	(tokens nil))
    (while (< i n)
      (if (not (eq (aref control i) ?~))
	  (incf i)
	  (when (> i start)
	    (push (substring control start i) tokens))
	  (let* ((at (1+ i))
		 (p (fmt-read-params control at))
		 (colonp nil)
		 (atp nil))
	    (setq i (cdr p))
	    (while (and (< i n) (memq (aref control i) '(?: ?@)))
	      (if (eq (aref control i) ?:) (setq colonp t) (setq atp t))
	      (incf i))
	    (when (>= i n)
	      (fmt-error control at "Directive is unterminated"))
	    (let ((char (upcase (aref control i))))
	      (incf i)
	      (cond
		((eq char ?\n)
		 ;; ~newline: skip the newline and the whitespace after it.
		 (when atp
		   (push "\n" tokens))
		 (unless colonp
		   (while (and (< i n) (memq (aref control i) '(?\s ?\t)))
		     (incf i))))
		(t
		 (let ((dir (make-fmt-dir char (car p) colonp atp)))
		   (when (eq char ?/)
		     (let ((end (string-match "/" control i)))
		       (unless end
			 (fmt-error control i "Missing / after ~/"))
		       (setf (fmt-dir-name dir) (substring control i end))
		       (setq i (1+ end))))
		   (push dir tokens))))))
	  (setq start i)))
    (when (> n start)
      (push (substring control start) tokens))
    (nreverse tokens)))

(defvar fmt-rest)

(defun fmt-build-seq (control terminators)
  "Collect items until a directive in TERMINATORS.
Return (ITEMS . TERMINATOR)."
  (let ((items nil)
	(term nil))
    (while (and fmt-rest (null term))
      (let ((token (pop fmt-rest)))
	(cond
	  ((stringp token)
	   (push token items))
	  ((memq (fmt-dir-char token) terminators)
	   (setq term token))
	  ((memq (fmt-dir-char token) '(?\[ ?\{ ?\( ?<))
	   (fmt-build-compound control token)
	   (push token items))
	  ((memq (fmt-dir-char token) '(?\] ?\} ?\) ?> ?\;))
	   (fmt-error control 0 (format "Unmatched ~%c" (fmt-dir-char token))))
	  (t
	   (push token items)))))
    (cons (nreverse items) term)))

(defun fmt-build-compound (control dir)
  (let* ((char (fmt-dir-char dir))
	 (close (cdr (assq char '((?\[ . ?\]) (?\{ . ?\}) (?\( . ?\)) (?< . ?>)))))
	 (terminators (if (memq char '(?\[ ?<)) (list ?\; close) (list close)))
	 (clauses nil)
	 (separators nil)
	 (done nil))
    (while (not done)
      (let ((r (fmt-build-seq control terminators)))
	(push (car r) clauses)
	(cond
	  ((null (cdr r))
	   (fmt-error control 0 (format "Missing ~%c" close)))
	  ((eq (fmt-dir-char (cdr r)) close)
	   (setf (fmt-dir-close dir) (cdr r))
	   (setq done t))
	  (t
	   (push (cdr r) separators)))))
    (setf (fmt-dir-clauses dir) (nreverse clauses)
	  (fmt-dir-separators dir) (nreverse separators))))

(defun fmt-parse (control)
  (or (gethash control fmt-cache)
      (puthash (copy-sequence control)
	       (let ((fmt-rest (fmt-tokens control)))
		 (car (fmt-build-seq control nil)))
	       fmt-cache)))

;;; Arguments and parameters ----------------------------------------

(defun fmt-args-left ()
  (- (length fmt-args) fmt-pos))

(defun fmt-next-arg ()
  (when (>= fmt-pos (length fmt-args))
    (ERROR "Not enough arguments for FORMAT"))
  (prog1 (aref fmt-args fmt-pos)
    (incf fmt-pos)))

(defun fmt-peek-arg ()
  (when (>= fmt-pos (length fmt-args))
    (ERROR "Not enough arguments for FORMAT"))
  (aref fmt-args fmt-pos))

(defun fmt-params (dir)
  "Resolve the parameters of DIR: V and # take arguments, 'c gives a
character code."
  (mapcar (lambda (p)
	    (cond
	      ((eq p 'V)
	       (let ((arg (fmt-next-arg)))
		 (if (CHARACTERP arg) (CHAR-CODE arg) arg)))
	      ((eq p 'N)	(fmt-args-left))
	      ((eq (car-safe p) 'quote)
				(cadr p))
	      (t		p)))
	  (fmt-dir-params dir)))

(defun fmt-string (x)
  (if (stringp x) x (el-string x)))

(defun fmt-princ (x)
  (if (stringp x) x (fmt-string (PRINC-TO-STRING x))))

(defun fmt-prin1 (x)
  (fmt-string (PRIN1-TO-STRING x)))

(defun fmt-pad (string mincol colinc minpad padchar left)
  (let* ((mincol (or mincol 0))
	 (colinc (max 1 (or colinc 1)))
	 (minpad (or minpad 0))
	 (width (+ (length string) minpad)))
    (when (< width mincol)
      (incf width (* colinc (ceiling (- mincol width) colinc))))
    (let ((pad (make-string (- width (length string)) (or padchar ?\s))))
      (insert (if left (concat pad string) (concat string pad))))))

;;; Output of numbers ----------------------------------------

(defun fmt-group-digits (digits commachar interval)
  (let ((groups nil)
	(i (length digits)))
    (while (> i 0)
      (push (substring digits (max 0 (- i interval)) i) groups)
      (decf i interval))
    (mapconcat #'identity groups (string commachar))))

(defun fmt-integer (dir radix params)
  (let ((arg (fmt-next-arg))
	(mincol (nth 0 params))
	(padchar (nth 1 params))
	(commachar (or (nth 2 params) ?,))
	(interval (or (nth 3 params) 3)))
    (if (not (INTEGERP arg))
	(let ((*PRINT-BASE* 10)
	      (*PRINT-RADIX* nil))
	  (fmt-pad (fmt-princ arg) mincol 1 0 padchar t))
	(let* ((minusp (MINUSP arg))
	       (digits (if (and (integerp arg) (eq radix 10))
			   (number-to-string (abs arg))
			   (fmt-string
			    (WRITE-TO-STRING (if minusp (cl:- arg) arg)
					     (kw BASE) radix (kw RADIX) nil
					     (kw ESCAPE) nil (kw READABLY) nil))))
	       (digits (if (fmt-dir-colonp dir)
			   (fmt-group-digits digits commachar interval)
			   digits)))
	  (fmt-pad (concat (cond (minusp "-")
				 ((fmt-dir-atp dir) "+")
				 (t ""))
			   digits)
		   mincol 1 0 padchar t)))))

(defconst fmt-ones
  ["" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
   "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen"
   "seventeen" "eighteen" "nineteen"])

(defconst fmt-tens
  ["" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty"
   "ninety"])

(defconst fmt-illions
  ["" " thousand" " million" " billion" " trillion" " quadrillion"
   " quintillion" " sextillion" " septillion" " octillion" " nonillion"
   " decillion"])

(defun fmt-cardinal-below-1000 (n)
  (let ((parts nil))
    (when (>= n 100)
      (push (concat (aref fmt-ones (/ n 100)) " hundred") parts)
      (setq n (% n 100)))
    (cond
      ((>= n 20)
       (push (concat (aref fmt-tens (/ n 10))
		     (if (zerop (% n 10)) "" (concat "-" (aref fmt-ones (% n 10)))))
	     parts))
      ((> n 0)
       (push (aref fmt-ones n) parts)))
    (mapconcat #'identity (nreverse parts) " ")))

(defun fmt-cardinal (n)
  (cond
    ((zerop n) "zero")
    ((< n 0) (concat "negative " (fmt-cardinal (- n))))
    (t
     (let ((parts nil)
	   (i 0))
       (while (> n 0)
	 (when (>= i (length fmt-illions))
	   (ERROR "Number too large to print in English: ~D" n))
	 (let ((group (% n 1000)))
	   (unless (zerop group)
	     (push (concat (fmt-cardinal-below-1000 group) (aref fmt-illions i))
		   parts)))
	 (setq n (/ n 1000))
	 (incf i))
       (mapconcat #'identity parts " ")))))

(defun fmt-ordinal (n)
  (let ((cardinal (fmt-cardinal n)))
    (cond
      ((string-match "\\(one\\|two\\|three\\|five\\|eight\\|nine\\|twelve\\|y\\)\\'"
		     cardinal)
       (concat (substring cardinal 0 (match-beginning 0))
	       (cdr (assoc (match-string 1 cardinal)
			   '(("one" . "first") ("two" . "second")
			     ("three" . "third") ("five" . "fifth")
			     ("eight" . "eighth") ("nine" . "ninth")
			     ("twelve" . "twelfth") ("y" . "ieth"))))))
      (t
       (concat cardinal "th")))))

(defun fmt-roman (n old)
  (unless (and (integerp n) (< 0 n 4000))
    (ERROR "Number out of range for ~~@R: ~D" n))
  (let ((table (if old
		   '((1000 . "M") (500 . "D") (100 . "C") (50 . "L")
		     (10 . "X") (5 . "V") (1 . "I"))
		   '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
		     (100 . "C") (90 . "XC") (50 . "L") (40 . "XL")
		     (10 . "X") (9 . "IX") (5 . "V") (4 . "IV") (1 . "I"))))
	(result ""))
    (dolist (entry table result)
      (while (>= n (car entry))
	(setq result (concat result (cdr entry))
	      n (- n (car entry)))))))

(defun fmt-radix (dir params)
  (if (car params)
      (fmt-integer dir (car params) (cdr params))
      (let ((arg (fmt-next-arg))
	    (colonp (fmt-dir-colonp dir))
	    (atp (fmt-dir-atp dir)))
	(insert (cond
		  (atp		(fmt-roman arg colonp))
		  (colonp	(fmt-ordinal arg))
		  (t		(fmt-cardinal arg)))))))

(defun fmt-float-arg (arg)
  (and (REALP arg) (FLOAT arg)))

(defun fmt-free-float (x)
  (let ((s (number-to-string x)))
    (if (string-match "e" s)
	(let ((f (format "%f" x)))
	  (if (string-match "\\.?0+\\'" f)
	      (concat (substring f 0 (match-beginning 0)) ".0")
	      f))
	s)))

(defun fmt-fixed-digits (d x)
  (format (format "%%.%df" d) x))

(defun fmt-fixed (dir params)
  (let* ((arg (fmt-next-arg))
	 (x (fmt-float-arg arg))
	 (w (nth 0 params))
	 (d (nth 1 params))
	 (k (or (nth 2 params) 0))
	 (overflow (nth 3 params))
	 (padchar (or (nth 4 params) ?\s)))
    (if (null x)
	(fmt-pad (fmt-princ arg) w 1 0 ?\s t)
	(let* ((x (* x (expt 10.0 k)))
	       (sign (cond ((< x 0) "-") ((fmt-dir-atp dir) "+") (t "")))
	       (ax (abs x))
	       (digits
		(cond
		  (d	(fmt-fixed-digits d ax))
		  (w	(let ((int (length (format "%.0f" ax))))
			  (fmt-fixed-digits (max 1 (- w int 1 (length sign))) ax)))
		  (t	(fmt-free-float ax))))
	       (s (concat sign digits)))
	  (when (and w (> (length s) w) (string-prefix-p "0." digits))
	    (setq s (concat sign (substring digits 1))))
	  (if (and w overflow (> (length s) w))
	      (insert (make-string w overflow))
	      (fmt-pad s w 1 0 padchar t))))))

(defun fmt-exponential (dir params)
  (let* ((arg (fmt-next-arg))
	 (x (fmt-float-arg arg))
	 (w (nth 0 params))
	 (d (nth 1 params))
	 (e (nth 2 params))
	 (overflow (nth 4 params))
	 (padchar (or (nth 5 params) ?\s))
	 (marker (or (nth 6 params) ?e)))
    (if (null x)
	(fmt-pad (fmt-princ arg) w 1 0 ?\s t)
	(let* ((sign (cond ((< x 0) "-") ((fmt-dir-atp dir) "+") (t "")))
	       (s (format (if d (format "%%.%de" d) "%e") (abs x))))
	  (string-match "\\`\\([0-9.]+\\)e\\([-+]\\)0*\\([0-9]+\\)\\'" s)
	  (let* ((mantissa (match-string 1 s))
		 (mantissa (if d
			       mantissa
			       (replace-regexp-in-string "\\([0-9]\\)0+\\'" "\\1"
							 mantissa)))
		 (exponent (match-string 3 s))
		 (exponent (if (and e (< (length exponent) e))
			       (concat (make-string (- e (length exponent)) ?0)
				       exponent)
			       exponent))
		 (s (concat sign mantissa (string marker) (match-string 2 s)
			    exponent)))
	    (if (and w overflow (> (length s) w))
		(insert (make-string w overflow))
		(fmt-pad s w 1 0 padchar t)))))))

(defun fmt-general (dir params)
  (let ((x (fmt-float-arg (fmt-peek-arg))))
    (if (and x (or (zerop x) (and (>= (abs x) 0.1) (< (abs x) 1e7))))
	(let ((w (nth 0 params)))
	  (fmt-fixed dir (list (and w (max 0 (- w 4))) (nth 1 params) nil
			       (nth 4 params) (nth 5 params)))
	  (insert "    "))
	(fmt-exponential dir params))))

(defun fmt-dollars (dir params)
  (let* ((arg (fmt-next-arg))
	 (x (fmt-float-arg arg))
	 (d (or (nth 0 params) 2))
	 (n (or (nth 1 params) 1))
	 (w (or (nth 2 params) 0))
	 (padchar (or (nth 3 params) ?\s)))
    (if (null x)
	(fmt-pad (fmt-princ arg) w 1 0 ?\s t)
	(let* ((sign (cond ((< x 0) "-") ((fmt-dir-atp dir) "+") (t "")))
	       (digits (fmt-fixed-digits d (abs x)))
	       (int (length (car (split-string digits "\\."))))
	       (digits (if (< int n)
			   (concat (make-string (- n int) ?0) digits)
			   digits))
	       (pad (max 0 (- w (length sign) (length digits)))))
	  (insert (if (fmt-dir-colonp dir)
		      (concat sign (make-string pad padchar) digits)
		      (concat (make-string pad padchar) sign digits)))))))

;;; Interpreter ----------------------------------------

(defun fmt-run (items)
  (dolist (item items)
    (if (stringp item)
	(insert item)
	(fmt-directive item))))

(defun fmt-fresh-line ()
  (cond
    ((> (point) (point-min))
     (unless (bolp)
       (insert "\n")))
    (fmt-stream
     (FRESH-LINE fmt-stream))))

(defun fmt-tabulate (dir params)
  (let ((col (current-column)))
    (if (fmt-dir-atp dir)
	(let ((colrel (or (nth 0 params) 1))
	      (colinc (or (nth 1 params) 1)))
	  (insert (make-string colrel ?\s))
	  (setq col (+ col colrel))
	  (when (> colinc 0)
	    (insert (make-string (% (- colinc (% col colinc)) colinc) ?\s))))
	(let ((colnum (or (nth 0 params) 1))
	      (colinc (or (nth 1 params) 1)))
	  (cond
	    ((< col colnum)
	     (insert (make-string (- colnum col) ?\s)))
	    ((> colinc 0)
	     (insert (make-string (- colinc (% (- col colnum) colinc)) ?\s))))))))

(defun fmt-goto (dir params)
  (let ((n (car params)))
    (cond
      ((fmt-dir-atp dir)
       (setq fmt-pos (or n 0)))
      ((fmt-dir-colonp dir)
       (decf fmt-pos (or n 1)))
      (t
       (incf fmt-pos (or n 1))))
    (unless (<= 0 fmt-pos (length fmt-args))
      (ERROR "FORMAT ~~* moved to argument ~D of ~D" fmt-pos (length fmt-args)))))

(defun fmt-recursive (dir)
  (let ((control (fmt-next-arg)))
    (if (fmt-dir-atp dir)
	(fmt-control control)
	(let ((fmt-args (vconcat (fmt-next-arg)))
	      (fmt-pos 0))
	  (catch 'fmt-escape
	    (fmt-control control))))))

(defun fmt-control (control)
  "Run CONTROL, a control string or a function from FORMATTER."
  (if (FUNCTIONP control)
      (let ((stream (MAKE-STRING-OUTPUT-STREAM)))
	(APPLY control stream (append (substring fmt-args fmt-pos) nil))
	(setq fmt-pos (length fmt-args))
	(insert (fmt-string (GET-OUTPUT-STREAM-STRING stream))))
      (fmt-run (fmt-parse (fmt-string control)))))

(defun fmt-plural (dir)
  (when (fmt-dir-colonp dir)
    (decf fmt-pos))
  (let ((plural (not (eql (fmt-next-arg) 1))))
    (insert (if (fmt-dir-atp dir)
		(if plural "ies" "y")
		(if plural "s" "")))))

(defun fmt-character (dir)
  (let ((char (fmt-next-arg)))
    (cond
      ((fmt-dir-colonp dir)
       (let ((code (CHAR-CODE char)))
	 (if (and (> code 32) (GRAPHIC-CHAR-P char))
	     (insert code)
	     (insert (fmt-string (CHAR-NAME char))))))
      ((fmt-dir-atp dir)
       (insert (fmt-prin1 char)))
      (t
       (insert (CHAR-CODE char))))))

(defun fmt-conditional (dir params)
  (let ((clauses (fmt-dir-clauses dir)))
    (cond
      ((fmt-dir-colonp dir)
       (fmt-run (if (fmt-next-arg) (nth 1 clauses) (nth 0 clauses))))
      ((fmt-dir-atp dir)
       (if (fmt-peek-arg)
	   (fmt-run (nth 0 clauses))
	   (incf fmt-pos)))
      (t
       (let* ((n (or (car params) (fmt-next-arg)))
	      (default (let ((last (car (last (fmt-dir-separators dir)))))
			 (and last (fmt-dir-colonp last)))))
	 (cond
	   ((and (integerp n) (<= 0 n) (< n (length clauses))
		 (not (and default (= n (1- (length clauses))))))
	    (fmt-run (nth n clauses)))
	   (default
	    (fmt-run (car (last clauses))))))))))

(defun fmt-iterate (dir params)
  (let* ((max (car params))
	 (body (car (fmt-dir-clauses dir)))
	 (body (if body
		   body
		   (fmt-parse (fmt-string (fmt-next-arg)))))
	 (once (fmt-dir-colonp (fmt-dir-close dir)))
	 (count 0))
    (if (fmt-dir-colonp dir)
	;; Each argument is a list of arguments for one iteration.
	(let ((lists (if (fmt-dir-atp dir)
			 (prog1 (append (substring fmt-args fmt-pos) nil)
			   (setq fmt-pos (length fmt-args)))
			 (append (fmt-next-arg) nil))))
	  (catch 'fmt-escape-all
	    (while (and (or lists (and once (zerop count)))
			(or (null max) (< count max)))
	      (let ((fmt-args (vconcat (pop lists)))
		    (fmt-pos 0)
		    (fmt-outer (length lists)))
		(when (eq (catch 'fmt-escape (fmt-run body) nil) 'colon)
		  (throw 'fmt-escape-all nil)))
	      (incf count))))
	(let ((run (lambda ()
		     (catch 'fmt-escape
		       (while (and (or (< fmt-pos (length fmt-args))
				       (and once (zerop count)))
				   (or (null max) (< count max)))
			 (let ((fmt-outer nil))
			   (fmt-run body))
			 (incf count))))))
	  (if (fmt-dir-atp dir)
	      (funcall run)
	      (let ((fmt-args (vconcat (fmt-next-arg)))
		    (fmt-pos 0))
		(funcall run)))))))

(defun fmt-escape (dir params)
  (let ((exit (cond
		((null params)
		 (if (fmt-dir-colonp dir)
		     (and fmt-outer (zerop fmt-outer))
		     (>= fmt-pos (length fmt-args))))
		((null (cdr params))
		 (eql (nth 0 params) 0))
		((null (cddr params))
		 (eql (nth 0 params) (nth 1 params)))
		(t
		 (<= (nth 0 params) (nth 1 params) (nth 2 params))))))
    (when exit
      (throw 'fmt-escape (if (fmt-dir-colonp dir) 'colon t)))))

(defun fmt-case (dir)
  (let ((start (point))
	(escape (catch 'fmt-escape
		  (fmt-run (car (fmt-dir-clauses dir)))
		  nil)))
    (cond
      ((and (fmt-dir-colonp dir) (fmt-dir-atp dir))
       (upcase-region start (point)))
      ((fmt-dir-colonp dir)
       (capitalize-region start (point)))
      ((fmt-dir-atp dir)
       (downcase-region start (point))
       (save-excursion
	 (goto-char start)
	 (when (re-search-forward "[[:alnum:]]" nil t)
	   (upcase-region (1- (point)) (point)))))
      (t
       (downcase-region start (point))))
    (when escape
      (throw 'fmt-escape escape))))

(defun fmt-render-clause (items)
  "Run ITEMS and return their output, or nil if ~^ escaped."
  (let ((start (point)))
    (if (catch 'fmt-escape (fmt-run items) nil)
	(progn (delete-region start (point)) nil)
	(prog1 (buffer-substring start (point))
	  (delete-region start (point))))))

(defun fmt-justify (dir params)
  (if (fmt-dir-colonp (fmt-dir-close dir))
      ;; ~<...~:> is a logical block; there is no pretty printer, so
      ;; just print its prefix, body and suffix.
      (let* ((clauses (fmt-dir-clauses dir))
	     (prefix (if (cdr clauses) (car clauses) nil))
	     (body (if (cdr clauses) (cadr clauses) (car clauses)))
	     (suffix (nth 2 clauses)))
	(fmt-run prefix)
	(if (fmt-dir-atp dir)
	    (catch 'fmt-escape (fmt-run body))
	    (let ((fmt-args (vconcat (fmt-next-arg)))
		  (fmt-pos 0))
	      (catch 'fmt-escape (fmt-run body))))
	(fmt-run suffix))
      (let* ((mincol (or (nth 0 params) 0))
	     (colinc (max 1 (or (nth 1 params) 1)))
	     (minpad (or (nth 2 params) 0))
	     (padchar (or (nth 3 params) ?\s))
	     (clauses (fmt-dir-clauses dir))
	     (first-sep (car (fmt-dir-separators dir)))
	     (clauses (if (and first-sep (fmt-dir-colonp first-sep))
			  (cdr clauses)
			  clauses))
	     (segments nil))
	(catch 'done
	  (dolist (clause clauses)
	    (let ((text (fmt-render-clause clause)))
	      (if text
		  (push text segments)
		  (throw 'done nil)))))
	(setq segments (nreverse segments))
	(let* ((gaps (+ (max 0 (1- (length segments)))
			(if (fmt-dir-colonp dir) 1 0)
			(if (fmt-dir-atp dir) 1 0)))
	       (left (or (fmt-dir-colonp dir)
			 (and (zerop gaps) (setq gaps 1))))
	       (text (apply #'+ (mapcar #'length segments)))
	       (width (+ text (* gaps minpad))))
	  (when (< width mincol)
	    (incf width (* colinc (ceiling (- mincol width) colinc))))
	  (let* ((pad (- width text))
		 (pads (let ((result nil))
			 (dotimes (i gaps (nreverse result))
			   (push (+ (/ pad gaps)
				    (if (>= i (- gaps (% pad gaps))) 1 0))
				 result)))))
	    (when left
	      (insert (make-string (pop pads) padchar)))
	    (while segments
	      (insert (pop segments))
	      (when (and pads (or segments (fmt-dir-atp dir)))
		(insert (make-string (pop pads) padchar)))))))))

(defun fmt-call (dir params)
  (let* ((name (fmt-dir-name dir))
	 (symbol (let ((*PACKAGE* (FIND-PACKAGE "CL-USER")))
		   (READ-FROM-STRING name)))
	 (stream (MAKE-STRING-OUTPUT-STREAM)))
    (APPLY (FDEFINITION symbol) stream (fmt-next-arg)
	   (if (fmt-dir-colonp dir) 'T nil)
	   (if (fmt-dir-atp dir) 'T nil)
	   params)
    (insert (fmt-string (GET-OUTPUT-STREAM-STRING stream)))))

(defun fmt-directive (dir)
  (let* ((char (fmt-dir-char dir))
	 (params (fmt-params dir))
	 (n (or (car params) 1)))
    (case char
      (?A (let ((arg (fmt-next-arg)))
	    (fmt-pad (if (and (null arg) (fmt-dir-colonp dir)) "()" (fmt-princ arg))
		     (nth 0 params) (nth 1 params) (nth 2 params) (nth 3 params)
		     (fmt-dir-atp dir))))
      (?S (let ((arg (fmt-next-arg)))
	    (fmt-pad (if (and (null arg) (fmt-dir-colonp dir)) "()" (fmt-prin1 arg))
		     (nth 0 params) (nth 1 params) (nth 2 params) (nth 3 params)
		     (fmt-dir-atp dir))))
      (?W (let ((arg (fmt-next-arg)))
	    (if (fmt-dir-atp dir)
		(let ((*PRINT-LEVEL* nil) (*PRINT-LENGTH* nil))
		  (insert (fmt-string (WRITE-TO-STRING arg))))
		(insert (fmt-string (WRITE-TO-STRING arg))))))
      (?D (fmt-integer dir 10 params))
      (?B (fmt-integer dir 2 params))
      (?O (fmt-integer dir 8 params))
      (?X (fmt-integer dir 16 params))
      (?R (fmt-radix dir params))
      (?C (fmt-character dir))
      (?F (fmt-fixed dir params))
      (?E (fmt-exponential dir params))
      (?G (fmt-general dir params))
      (?$ (fmt-dollars dir params))
      (?% (insert (make-string n ?\n)))
      (?& (when (> n 0)
	    (fmt-fresh-line)
	    (insert (make-string (1- n) ?\n))))
      (?| (insert (make-string n ?\f)))
      (?~ (insert (make-string n ?~)))
      (?T (fmt-tabulate dir params))
      (?* (fmt-goto dir params))
      (?? (fmt-recursive dir))
      (?P (fmt-plural dir))
      (?\[ (fmt-conditional dir params))
      (?\{ (fmt-iterate dir params))
      (?^ (fmt-escape dir params))
      (?\( (fmt-case dir))
      (?< (fmt-justify dir params))
      (?/ (fmt-call dir params))
      (?_ (when (and (fmt-dir-colonp dir) (fmt-dir-atp dir))
	    (insert "\n")))
      (?I nil)
      (t (ERROR "Unknown FORMAT directive ~~~A" (CODE-CHAR char))))))

(defun fmt-output (control args stream)
  "Return the output of CONTROL with ARGS as a string."
  (with-temp-buffer
    (let ((fmt-args (vconcat args))
	  (fmt-pos 0)
	  (fmt-stream stream)
	  (fmt-outer nil))
      (catch 'fmt-escape
	(fmt-control control))
      (buffer-string))))



(cl:defmacro FORMATTER (format)
  (unless (STRINGP format)
    (type-error format 'STRING))
  ;; TODO: better implementation
  (let ((env (augment-environment nil :variable '(format))))
    (setf (lexical-value 'format env) format)
    (enclose '(LAMBDA (*STANDARD-OUTPUT* &REST args)
	        (APPLY (FUNCTION FORMAT) T format args)
	        nil)
	     env (format "\"formatter \"%s\"\"" format))))

(defun FORMAT (stream-designator format &rest args)
  (let ((stream (cond
		  ((eq stream-designator 'T)	*STANDARD-OUTPUT*)
		  ((STRINGP stream-designator)	nil)
		  (t				stream-designator))))
    (if (and stream (FUNCTIONP format))
	(progn (APPLY format stream args) nil)
	(let ((text (fmt-output format args stream)))
	  (cond
	    ((null stream-designator)
	     text)
	    ((STRINGP stream-designator)
	     (dotimes (i (length text))
	       (VECTOR-PUSH-EXTEND (CODE-CHAR (aref text i)) stream-designator))
	     nil)
	    (t
	     (WRITE-STRING text stream)
	     nil))))))
