
build : build-main.lisp Makefile
	sbcl --disable-ldb --disable-debugger --load build-main.lisp

realclean : clean
	rm -f build

clean:
	rm -f *~ *.fasl *.o file
