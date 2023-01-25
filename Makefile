LISP ?= ecl

all: test

build:
	$(LISP)	--non-interactive \
		--load csv-validator.asd \
		--eval '(ql:quickload :csv-validator)' \
		--eval '(asdf:make :csv-validator)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp
