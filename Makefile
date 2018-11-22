stringLinker: stringLinker.hs
	ghc --make stringLinker.hs

.PHONY: clean
clean:
	rm stringLinker.hi stringLinker.o stringLinker
	cd test && $(MAKE) clean

.PHONY: test
test: stringLinker
	cd test && $(MAKE) -k test

