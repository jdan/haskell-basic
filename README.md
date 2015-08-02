A [TinyBASIC](https://en.wikipedia.org/wiki/Tiny_BASIC) interpreter written in Haskell compatible with [GHCJS](https://github.com/ghcjs/ghcjs)

## console

```
$ make build
$ cat test.bas
10 LET A = 3
20 LET B = (A*5) + 2
30 PRINT B
$ cat test.bas | ./basic-interpreter
17
```

## js

```
$ make js
$ cd basic-interpreter.jsexe
$ python -m SimpleHTTPServer
```
