EMACS ?= emacs

.PHONY: test compile check

# Tests of emacs-cl.el, running Common Lisp in a batch Emacs.
test:
	$(EMACS) -Q --batch -L . -l test/emacs-cl-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc file is removed again, as it would shadow the source
# of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile emacs-cl.el; \
	status=$$?; rm -f emacs-cl.elc; exit $$status

# Common Lisp's own tests, cl/tests.lisp, compiled into build/.
check:
	cd cl && $(EMACS) -Q --batch -l ../emacs-cl-inferior.el --source . --build ../build \
	  --eval '(load "tests.lisp")'; rm -f temporary.lisp temporary.elc
