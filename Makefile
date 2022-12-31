LISP ?= sbcl

all: test

run:
	$(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load stream_validator.asd \
		--eval '(ql:quickload :stream_validator)' \
		--eval '(asdf:make :stream_validator)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp
