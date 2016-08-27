all:
	ghc -threaded Main.hs

run: all
	./Main

clean:
	rm *.o
	rm *.hi
	rm Main
