# Right-linear grammar to FSM

At the beginning it is worth to mention the definition of alphabet and grammar.
- The alphabet is a finite non-empty set of elements we call symbols.
- A grammar is a foursome that consists of a non-terminal alphabet, a terminal alphabet, a finite set of rules, and an initial non-terminal.

Thus, according to these definitions, the program allows to process grammar without rules, since an empty set is also a set. Non-terminals and terminals cannot be empty because they are defined as an alphabet, not directly as a set.

Grammar is treated that it cannot contain simple rules of type `A->B`, terminals must be in the range `a-z` and non-terminals in the range `A-Z`. At the same time it is treated whether the initial symbol belongs to the alphabet of non-terminals and whether all symbols in the rules are defined. In addition, the shape of the rules is treated to match the shape of the RLG. No duplications are allowed, as they do not occur in the set.

## Installation
- For compilation use `make`.

``` 
./plg-2-nka < -i | -1 | -2 > [file]
```

- **-i** : prints the loaded grammar
- **-1** : prints the transformed grammar
- **-2** : prints an equivalent finite state machine

The input grammar can be specified in a file or on the standard input. The shape is defined as follows:
- Line 1: Comma-separated non-terminals.
- Line 2: Comma-separated terminals.
- Line 3: Initial non-terminal.
- Other lines contain rules, where each rule is defined on one line.

The output non-deterministic finite state machine is defined as follows:
- Line 1: Comma-separated states.
- Line 2: Comma-separated input alphabet.
- Line 3: Initial state.
- Line 4: Comma-separated finite states.
- Other lines contain rules, where each rule is defined on one line.

Examples of formats can be found in the `test` directory.
