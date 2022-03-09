.PHONY: all

all: build/cacl-sbcl build/cacl-ccl build/cacl-abcl build/cacl.1

# Build -----------------------------------------------------------------------
lisps := $(shell ffind '\.(asd|lisp)$$')

build:
	mkdir -p build

build/cacl-sbcl: build $(lisps)
	sbcl --load "src/build-binary.lisp"

build/cacl-ccl: build $(lisps) bin/cacl-ccl
	ccl --load "src/build-binary.lisp"
	cp bin/cacl-ccl build/

build/cacl-abcl: build $(lisps) bin/cacl-abcl
	cp bin/cacl-abcl build/

build/cacl.1: build $(lisps)
	sbcl --load "src/build-manual.lisp" --quit
