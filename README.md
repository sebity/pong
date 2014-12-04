Classic Pong Remake in Common Lisp

## Dependencies

- [LISPBUILDER-SDL](https://code.google.com/p/lispbuilder/wiki/LispbuilderSDL)

## Quickstart

To run this game place the files somewhere [Quicklisp][] can find it, and execute the following in the REPL:

```lisp
(ql:quickload :pong)
(pong:start)
```
The control the paddle use the `A` and `Z` keys to go up and down.
