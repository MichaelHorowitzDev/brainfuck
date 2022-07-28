# brainfuck

This is a brainfuck compiler / interpreter written in Haskell. It uses a very simple method to parse commands and convert them into an abstract syntax tree which are then either compiled or executed in the terminal.

# Compiler
The target compile language is C. No optimization is done by the compiler. It's just a strict 1 to 1 conversion.

# Interpreter / REPL
The main part of this program is the REPL (Read-Eval-Print Loop). It allows you to continually input brainfuck code and have it affect the current environment. Also features the ability to save and load the current session in CSV format (JSON to come soon, maybe).
