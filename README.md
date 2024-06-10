# Address Programming Language

A simple Haskell implementation of an interpreter for the Address Programming Language developed in the 1950s by Kateryna Lonhvynivna Yushchenko.

## Introduction

### Syntax

The syntax of ADPL is described [here](docs/syntax.md).

Also, some `.adpl` files in the `test/data` folder
have their original syntax equivalents in `.txt` files with the same name.

## Usage

### Dependencies

To use the interpreter, it is necessary to have a version of
**[Stack](https://docs.haskellstack.org/en/stable/)** installed locally.
**Stack** is a popular build tool for Haskell.

### Run

To execute an ADPL file, run the `stack run` command in the terminal with a path to the file as an argument.
For example, to execute the file `./test/data/fact.adpl`, run the following command:

```shell
stack run ./test/data/fact.adpl
```

There are currently some warnings displayed when running programs.
You can suppress them with the option `--silent`:

```shell
stack run --silent ./test/data/fact.adpl
```

### IDE support

There is a simple version of ADPL syntax highlighting available for VSCode:
**[AddLang](https://marketplace.visualstudio.com/items?itemName=jorg.addlang)**.
