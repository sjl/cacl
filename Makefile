.PHONY: vendor binaries

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Build -----------------------------------------------------------------------
lisps := $(shell ffind '\.(asd|lisp)$$')

binaries: cacl-sbcl cacl-ccl cacl-ecl cacl-abcl

cacl-sbcl: $(lisps)
	sbcl --load "src/build.lisp"
	mv cacl cacl-sbcl

cacl-ccl: $(lisps)
	ccl --load "src/build.lisp"
	mv cacl cacl-ccl

cacl-ecl: $(lisps) bin/cacl-ecl
	cp bin/cacl-ecl cacl-ecl

cacl-abcl: $(lisps) bin/cacl-abcl
	cp bin/cacl-abcl cacl-abcl
