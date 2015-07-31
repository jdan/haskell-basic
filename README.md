A TinyBASIC interpreter written in Haskell

```
$ ghc basic-parser.hs
$ cat test.bas
10 LET A = 3
20 LET B = (A*5) + 2
30 PRINT B
$ cat test.bas | ./basic-parser
17
```
