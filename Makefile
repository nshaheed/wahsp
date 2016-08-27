all:
	ghc -threaded -Wall intial_tests.hs

run: all
	./intial_tests

clean:
	rm *.o
	rm *.hi
	rm Main
