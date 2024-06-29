# Address Programming Language

A simple Haskell implementation of an interpreter for the Address Programming Language developed in the 1950s by Kateryna Lonhvynivna Yushchenko.

## Introduction

### Project Structure

This is a quick guide to the project structure:

- [src](src) - source code for ADPL interpreter
  - [Lib](src/Lib.hs) - library entry point
  - [Lexer](src/Lexer) - lexical analyzer for ADPL
  - [Parser](src/Parser) - parser for ADPL
  - [Value](src/Value) - data types for representing the values in ADPL
  - [ByteCode](src/ByteCode) - data types for representing ADPL bytecode
  - [Compiler](src/Compiler) - core logic for compiling from source code to ADPL bytecode
  - [Vm](src/Vm) - virtual machine for ADPL bytecode
  - [Utils](src/Utils) - common utils
- [test](test) - contains examples of programs in the original syntax and in ADPL
- [docs](docs) - documentation
  - [syntax](docs/syntax.md) - documentation for the ADPL syntax adaptation
  - [examples](docs/examples.md) - comparison between programs written in C++, original Address Programming language syntax, and ADPL

## Usage

### Dependencies

To use the interpreter, it is necessary to have a version of
**[Stack](https://docs.haskellstack.org/en/stable/)** installed locally.
**Stack** is a popular build tool for Haskell.

### Run

To execute an ADPL file, run the `stack run` command in the terminal with a path to the file as an argument.
For example, to execute the file `./test/data/fact.adpl`, run the following command:

```shell
stack run ./test/data/fact/fact.adpl
```

There are currently some warnings displayed when running programs.
You can suppress them with the option `--silent`:

```shell
stack run --silent ./test/data/fact/fact.adpl
```

### IDE support

There is a simple version of ADPL syntax highlighting available for VSCode:
**[AddLang](https://marketplace.visualstudio.com/items?itemName=jorg.addlang)**.
