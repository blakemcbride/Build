
build : build.lisp Makefile
	sbcl --disable-ldb --disable-debugger --load build.lisp

realclean : clean
	rm -f build

clean:
	rm -f *~ *.fasl *.o file
