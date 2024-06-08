# Address Programming Language

A simple Haskell implementation of an interpreter for the Address Programming Language developed in the 1950s by Kateryna Lonhvynivna Yushchenko.

## Introduction

The syntax of ADPL is described in [this file](docs/syntax.md).

## Usage

### Dependencies

To use the interpreter, it is necessary to have a version of
**[stack](https://docs.haskellstack.org/en/stable/)** installed locally.
**Stack** is a popular build tool for Haskell.

### Run

To run an ADPL file, run the ``stack run`` command in the terminal:

```shell
stack run ./test/data/fact.adpl
```