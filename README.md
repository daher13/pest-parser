# Pest Parser Interpreter

This project is an interpreter for grammars written in [Pest](https://pest.rs/), a Parsing Expression Grammar (PEG) parser generator for Rust. Implemented in [Racket](https://racket-lang.org/), it aims to parse and evaluate Pest grammar files, providing insights into their structure and behavior.

## Features

- **AST Generation**: Parses `.pest` grammar files and constructs their Abstract Syntax Trees (ASTs).
- **Grammar Interpretation**: Evaluates Pest grammars to understand their parsing logic.
- **Modular Design**: Separates concerns between AST construction and grammar interpretation.

## Project Structure

- `pest-ast.rkt`: Responsible for parsing Pest grammar files and generating their corresponding ASTs.
- `pest-interpreter.rkt`: Interprets the ASTs to simulate the behavior of the defined grammars.

## Getting Started

### Prerequisites

- [Racket](https://racket-lang.org/) installed on your system.

### Installation

1. Clone the repository:

   ```bash
   git clone https://github.com/daher13/pest-parser.git
   cd pest-parser

2. Run the interpreter:
```bash
racket pest-interpreter.rkt
```
