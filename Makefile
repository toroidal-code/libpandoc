GHC_VERSION:=$(shell echo `ghc --version | awk '{print $$NF}'`)

all: main

main:
	$(info GHC is version ${GHC_VERSION})
	$(shell sed -i 's/$$compiler/$(GHC_VERSION)/g' libpandoc.cabal)
	cabal configure
	cabal install --only-dependencies
	cabal build

demo: highlighting_demo

highlighting_demo: demo/highlighting.c
	$(CC) -lpandoc -o $@ $^

install:
	install -T dist/build/libpandoc.dll/libpandoc.dll /usr/lib/libpandoc.so
	install src/pandoc.h /usr/include/
	ldconfig

uninstall:
	rm -f /usr/lib/libpandoc.so
	rm -f /usr/include/pandoc.h
	ldconfig
