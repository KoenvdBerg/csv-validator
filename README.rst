csv-validator
=============

Are you looking to validate large heaps of data (in csv format)
quickly while still having the flexibility that Common Lisp offers?
This library is specifically designed to do that. It is most suitable
to support a scenario with incoming csv files that are expected to
have similar header and similar contents each delivery. See the
example below for how it's used:

GIF EXAMPLE

This README contains the sections mentioned below. If you'd like to
get started, hop over to the "Get started" section. A benchmark
demonstrating the speed for different file sizes is also included.

.. contents:: Table of Contents

Example

Benchmark
---------

This benchmark was performed using energy generation capacity data
from data-source_. Csv-files with filesizes up to 200MB were validated
using a validation suite with 18 or 36 validations and using 1 core or
4 cores. Each combination was sampled three times and averaged
thereafter. The results are shown in the table below:

.. image:: data/benchmark_results.png
  :width: 400
  :alt: Alternative text

	





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


.. _data-source: https://data.open-power-system-data.org/national_generation_capacity/2020-10-01
