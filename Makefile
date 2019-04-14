all: build build-manual.html

build : build-main.lisp Makefile
	sbcl --disable-ldb --disable-debugger --load build-main.lisp

build-manual.html : build-manual.adoc
	asciidoctor -a toc $<

realclean : clean
	rm -f build

clean:
	rm -f *~ *.fasl *.o file
