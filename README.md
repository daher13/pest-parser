# Regex Processor

**Regex Processor** is a regular expression interpreter written in [Racket](https://racket-lang.org/). This project includes a compiler, lexer, parser, virtual machine, and supporting tools to parse and evaluate regular expressions.

## Features

- **Regex Compilation**: Converts regular expressions into an intermediate representation.
- **Lexical and Syntactic Analysis**: Processes input using `lexer.rkt` and `parser.rkt`.
- **Custom Virtual Machine**: Executes compiled regular expressions against input strings.
- **Regex Derivation**: Implements algorithms for expression derivation and simplification.
- **Integrated Tests**: Includes test cases to validate components and behavior.

## Project Structure

- `compiler.rkt`: Handles the compilation of regular expressions.
- `lexer.rkt`: Performs lexical analysis.
- `parser.rkt`: Parses the regular expressions.
- `vm.rkt`: Defines the virtual machine for executing expressions.
- `derivate.rkt`: Contains functions for expression derivation.
- `main.rkt`: Main entry point of the application.
- `tests/`: Directory containing test files.

## Requirements

- [Racket](https://racket-lang.org/) must be installed on your system.

## How to Run

1. Clone the repository:

   ```bash
   git clone https://github.com/daher13/regex-processor.git
   cd regex-processor
