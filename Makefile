LISP ?= sbcl

all: test

run:
	$(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load csv-validator.asd \
		--eval '(ql:quickload :csv-validator)' \
		--eval '(asdf:make :csv-validator)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp
