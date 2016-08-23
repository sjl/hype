.PHONY: vendor

vendor: vendor/quickutils.lisp

vendor/quickutils.lisp: vendor/make-quickutils.lisp
	sbcl-rlwrap --noinform --load make-quickutils.lisp  --eval '(quit)'
