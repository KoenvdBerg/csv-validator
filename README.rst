csv-validator
=============

.. image:: images/example.gif
  :width: 925
  :alt: Alternative text
	      
This package outlines an attempt to define a way of thinking about
validating data in tabular format (CSV) similar to its Python
homologue Great Expectations. It's been optimized for performance and
flexibility. It works by defining a set of validations that can be
repeatedly applied to a tabular dataset in CSV format. The validations
are stored in a validation suite that is written entirely in Common
Lisp syntax.

**Features:**

- **Performant**: single core validation speed is ~10MB/s and
  multicore validation speed is ~30MB/s. Please see `Benchmark`_. 
- **Extensible**: the entire validation suite is written in Common
  Lisp. Please see `Get started`_.
- **No-nonsense output**: output is a CSV file that contains location
  of values that did not pass the validation, including the erronuous
  value.
- **Well tested**: each validation is unit-tested.


Get started
---------

Installation
~~~~~~~~~~

This package is in the process of being added to quicklisp. In the
meantime, install as follows:

1. Clone this repository::

     git clone  git@github.com:KoenvdBerg/csv-validator.git

2. Open the LISP REPL and run::

     (load "/path/to/csv-validator.asd")
     (ql:quickload 'csv-validator)

     # For development load this as well:
     (in-package :csv-validator)
3. Test if installation was succesfull by running::

     (csv-validator:check-not-null "test-value")

   If this returns ``T`` all is right. 


Validating a simple CSV
~~~~~~~~~~

In the ``data/`` folder the file ``energy_sample.csv`` is
located. This small csv file is used to illustrate how the
csv-validator works.

Let's start with defining a small validation suite. Open a new common
lisp script and make sure that the csv-validator is correctly
installed. Then define::


  (defparameter *test_suite*
   (list
    (list
    :column "ID"			;The column to be validated
    :depends (list "ID")		;The value to use in the logic (see below)
    :label "max-5-chars"		;The text to include in output in case of failed validation
    :logic (lambda (ID)			;The logic for this validation. 
	     (<= (length ID) 5)))))	;The value in the ID column should have 5 or less characters.

Load the defined validation suite ``*test-suite*`` in the REPL and
start the validation. Make sure that you include the correct path to
the ``energy_sample.csv`` input data and that the output folder that
you're writing to exists::

  (csv-validator:validate-csv "/path/to/data/energy_sample.csv"
  "/path/to/output/folder/" *test_suite* :delim #\;)

Now open the file named ``csv-validator_validations.csv`` in the output folder and explore the result::

  index;column;erronuous_value;label
  11;ID;999999;max-5-chars

As you can see the result itself is a CSV file (';' as delimiter). It
correctly points out that the value in the ``ID`` column at index 11
is longer than 5 characters.


Header validations
~~~~~~~~~~

Record validations
~~~~~~~~~~


Benchmark
---------

This benchmark was performed using:

+---------------------+-----------------------------------------------------------------------+
| CPU                 | AMD Ryzen™ 7 PRO 6850U with Radeon™ Graphics × 16                     |
+---------------------+-----------------------------------------------------------------------+
| LISP implementation | SBCL 2.2.11                                                           |
+---------------------+-----------------------------------------------------------------------+
| OS                  | Fedora Linux 37 (Workstation Edition)                                 |
+---------------------+-----------------------------------------------------------------------+
|                     | name: National generation capacity                                    |
|                     +-----------------------------------------------------------------------+
|                     | description: Aggregated generation capacity by technology and country |
| Dataset             +-----------------------------------------------------------------------+
|                     | consulted on: 2023-01-24                                              |
|                     +-----------------------------------------------------------------------+
|                     | link: source_                                                         |
+---------------------+-----------------------------------------------------------------------+

Csv-files with filesizes up to 200MB were validated using a validation
suite with 18 or 36 validations and using 1 core or 4 cores. Each
combination was sampled three times and averaged thereafter. The
results are shown in the table below:

+---------------+---------------------------------------------+
| filesize (MB) | time (s)                                    |
|               +----------------------+----------------------+
|               | 18 validations       | 36 validations       |
|               +----------+-----------+----------+-----------+
|               | 1 thread | 4 threads | 1 thread | 4 threads |
+===============+==========+===========+==========+===========+
| 0.0032        | 0.009    | 0.015     | 0.011    | 0.018     |
+---------------+----------+-----------+----------+-----------+
| 0.026         | 0.009    | 0.012     | 0.009    | 0.014     |
+---------------+----------+-----------+----------+-----------+
| 0.2754        | 0.019    | 0.019     | 0.029    | 0.021     |
+---------------+----------+-----------+----------+-----------+
| 1.8           | 0.122    | 0.062     | 0.205    | 0.090     |
+---------------+----------+-----------+----------+-----------+
| 6.8           | 0.428    | 0.185     | 0.748    | 0.289     |
+---------------+----------+-----------+----------+-----------+
| 13.6          | 0.838    | 0.349     | 1.466    | 0.540     |
+---------------+----------+-----------+----------+-----------+
| 109.2         | 6.623    | 2.369     | 11.765   | 3.846     |
+---------------+----------+-----------+----------+-----------+
| 218.4         | 13.259   | 4.712     | 23.908   | 7.647     |
+---------------+----------+-----------+----------+-----------+

Next the average speed (in MB/s) was calculated skipping filesizes
lower than 1MB. The table below shows the results:

+---------------+----------------------+
|               | average speed (MB/s) |
| n validations +----------+-----------+
|               | 1 thread | 4 threads |
+===============+==========+===========+
| 18            | 15.971   | 39.432    |
+---------------+----------+-----------+
| 36            | 9.109    | 25.139    |
+---------------+----------+-----------+

*Conclusions*

- For file sizes up to 1MB it doesn't make sense to run the
  csv-validator using multiple threads.
- The more checks are applied to the csv data, the slower the
  csv-validator is.

Contributing
---------

Tests are defined with [Fiveam](https://common-lisp.net/project/fiveam/docs/).

Run them from the terminal with `make test`. You should see a failing test.

On Slime, load the test package and run `run!`.

Licence: BSD


.. _source: https://data.open-power-system-data.org/national_generation_capacity/2020-10-01

YARD:

This validation suite contains 1 validation. The following fields are defined:

- **column**: Defines the column on which the validation will be
  performed. In this case the ``ID`` column.
- **depends**: A list that contains the columns from which the values
  should be obtained that are used in the ``logic``. 
- **label**: Text that will be included in the output in case of error.
- **logic**: Defines the logic for the bounds of the values in the
  ``column``. It does so by defining a function that has the exact
  amount of input argument as in the ``depends`` field. In this case
  that means 1 argument which is ``ID``.
