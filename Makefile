FILENAME=basic-interpreter.hs

build:
	ghc -cpp $(FILENAME)

js:
	ghcjs -cpp $(FILENAME)
