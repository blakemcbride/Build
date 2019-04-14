

(build-java "all-classes" "Kiss/src/main/java" "Kiss/out/production/classes" "Kiss/libs")


(depends "file" "file.o" 
	 (run "gcc" "-o" target out-of-date-dependencies))
	 
(depends "file.o" "file.c" 
	 (run "gcc" "-c" dependencies))

