# Gooli

Gooli is a reimplementation in Scheme of the GOO language developed by
Jonathan Bachrach. The language features:

* a SEXP based syntax
* a class based object system with a rich type system: singleton, ...
* multiple dispatch with generic functions

It is implemented with the Gambit-C system.

## Installing

The installaion of goo follows the standard GNU procedure:

    % ./configure
    % make
    % make install

## Running

Gooli supports evaluating forms in a REPL and evaluating an entire
file. Start a REPL with:

    % goo.scm

To evaluate a file, do:

    % goo.scm <file.goo>

