all:
	cabal install
	cd examples/;ghc -threaded Basic.hs

run: all
	cd examples/;./Basic

clean:
	rm examples/*.o
	rm examples/*.hi
	rm examples/Basic

uninstall:
	rm -r ~/.cabal/share/x86_64-linux-ghc-7.10.3/wahsp-0.2/
	rm -r ~/.cabal/share/doc/x86_64-linux-ghc-7.10.3/wahsp-0.2/
	rm ~/.ghc/x86_64-linux-7.10.3/package.conf.d/wahsp*
