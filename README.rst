csv-validator
=============

.. contents:: Table of Contents
   
Introductionary text here...

Example

explain why here

Benchmark
---------

Get started
---------

Installation
~~~~~~~~~~

Run from the repl:
1. open run.lisp
2. compile the first 2 commands
3. run (in-package :stream_validator) in the REPL


Run from sources:

    make run
    # aka sbcl --load run.lisp

choose your lisp:

    LISP=ccl make run

or build and run the binary:

```
$ make build
$ ./stream_validator [name]
Hello [name] from stream_validator
```

Running your first validation
~~~~~~~~~~

Creating validations
~~~~~~~~~~

Contributing
~~~~~~~~~~

Tests are defined with [Fiveam](https://common-lisp.net/project/fiveam/docs/).

Run them from the terminal with `make test`. You should see a failing test.

On Slime, load the test package and run `run!`.

Licence: BSD

