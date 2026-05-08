# typey

A simply-typed lambda calculus interpreter, written in Haskell. This is a learning project — I'm using it to get a feel for Haskell while working through some PL theory, such as learning to write a simply-typed lambda calculus interpreter.

It's not finished.

## What it is

A simply-typed lamba calculus: lambda binders carry type annotations, type checking happens before evaluation as a separate pass. The evaluator uses call-by-value small-step reduction. There are base types (`Int`, `Bool`) and function types. The type checker rejects ill-typed terms before they reach eval.

A lexer and parser are in progress. For now, expressions are constructed directly as AST values.

## Prerequisites

- GHC (tested with 9.4.8)
- cabal

## Building and running

```
cabal run
```

That's it. Output is whatever is in `main` — currently a handful of hardcoded expressions being evaluated and type-checked. When the parser is complete, I plan to implement the repl.
