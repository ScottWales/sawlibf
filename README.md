# A Fortran library

This is a basic Fortran libary with a few types

## Types

* String
* Linked List Template

## Usage

To build all binaries and libraries

    make

To build and run all tests

    make check

To build documentation using doxygen

    make doc

To remove all build artefacts

    make clean

## Source directory structure:

* src/bin: Each file will be compiled into a binary and put into `bin`.
* src/test: Each file will be compiled into a binary and put into `test`. The
  binaries will be run when calling `make check`
* src/lib: Each directory will be compiled into a static library and put under
  `lib`, the libraries will be linked with the binaries.

