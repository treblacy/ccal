ccal : ccal.hs
	ghc -O $<
	strip $@

.PHONY: clean
clean:
	rm -f ccal ccal.hi ccal.o
