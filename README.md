Pong
====

Classic Pong Remake in Common Lisp

## Dependencies

- [LISPBUILDER-SDL](https://code.google.com/p/lispbuilder/wiki/LispbuilderSDL)

## Quickstart

To run this game place the files somewhere [Quicklisp](http://www.quicklisp.org/) can find it, and execute the following in the REPL:

```lisp
(ql:quickload :pong)
(pong:start)
```

## Controls

To control the paddle use the `A` and `Z` keys for Player 1, and `Up` and `Down` arrows for player 2.
