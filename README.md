A TinyBASIC interpreter written in Haskell that works with GHCJS

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
