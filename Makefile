.PHONY: all vendor

all: vendor build/cacl-sbcl build/cacl-ccl build/cacl-abcl build/cacl-ecl build/cacl.1

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Build -----------------------------------------------------------------------
lisps := $(shell ffind '\.(asd|lisp)$$')

build:
	mkdir -p build

build/cacl-sbcl: build $(lisps)
	sbcl --load "src/build-binary.lisp"

build/cacl-ccl: build $(lisps) bin/cacl-ccl
	ccl --load "src/build-binary.lisp"
	cp bin/cacl-ccl build/

build/cacl-ecl: build $(lisps) bin/cacl-ecl
	cp bin/cacl-ecl build/

build/cacl-abcl: build $(lisps) bin/cacl-abcl
	cp bin/cacl-abcl build/

build/cacl.1: build $(lisps)
	sbcl --load "src/build-manual.lisp" --quit
