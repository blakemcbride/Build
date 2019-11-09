
; build.lisp
; written by Blake McBride (blake1024@gmail.com)

(defparameter *debugging* t)

(require :sb-posix)

; muffle all compiler warnings
(declaim (sb-ext:muffle-conditions cl:warning))

(if *debugging* (declaim (optimize (debug 3))))

; Set root directory for tests
(if *debugging*
    (let ((path "/home/blake/Build"))
      (setf *default-pathname-defaults* (truename path))
      (sb-posix:chdir path)))

;(require "asdf")
;(require :trivial-features)
;(require :alexandria)
;(require :babel)
;(require :cffi)


(defparameter *verbose* t
  "verbose output of commands")

(defparameter *build* nil
  "The build command")

(defparameter *build-args* nil
  "The arguments passed to the build program")

(defparameter *target-info* (make-hash-table :test 'equal :size 100000)
  "full-target-name - (file-date build-required build-required-known)")

(defparameter *build-clauses* nil
     "list of dependencies
      Each element looks like: 
         ('dependency target dependencies recipe-lambda)
             or
         ('corresponding target-name (target-root-path target-type) (dependency-root-path dependency-type)
                 (other-dependencies)
                 recipe-lambda)
")

; build clause accessor macros

(defmacro is-dependency (bc)
  "Return non-null if build clause is a corresponding build clause"
  `(eq 'dependency (car ,bc)))

(defmacro is-corresponding (bc)
  "Return non-null if build clause is a corresponding build clause"
  `(eq 'corresponding (car ,bc)))

; for dependency type

(defmacro d-target (bc)
  "Get target from build clause"
  `(cadr ,bc))

(defmacro d-deps (bc)
  "get the dependencies from a build clause"
  `(caddr ,bc))

(defmacro d-recipe (bc)
  "Gets recipe code from a build clause"
  `(cadddr ,bc))

(defmacro d-has-recipe (bc)
  "Is there a recipe?"
  `(cdddr ,bc))

; for corresponding type

(defmacro c-target (bc)
  "Get target name from a corresponding build clause"
  `(cadr ,bc))

(defmacro c-target-info (bc)
  "Get target info from corresponding build clause"
  `(caddr ,bc))

(defmacro c-dep-info (bc)
  "Get dependency info from corresponding build clause"
  `(cadddr ,bc))

(defmacro c-other-deps (bc)
  "Gets other-dependencies from a corresponding build clause"
  `(car (cddddr ,bc)))

(defmacro c-recipe (bc)
  "Gets the recipe from a corresponding build clause"
  `(cadr (cddddr ,bc)))

(defmacro c-has-recipe (bc)
  "Is there a recipe?"
  `(cdr (cddddr ,bc)))

(defmacro c-root-path (x)
  "Return the target or dependency root path"
  `(car ,x))

(defmacro c-type (x)
  "Return the target or dependency type"
  `(cadr ,x))

;; end


(defparameter *directory-stack* nil)

(defparameter *main-targets* nil
  "This is what is being built.  List of string targets.")

(defmacro caddddr (x)
  `(car (cddddr ,x)))

(defmacro cadaddr (x)
  `(car (cdaddr ,x)))

(defun add-target (name)
  "Adds name to *target-info* if it's not already registered
   Returns the name-specific node
"
  (declare (type string name))
  (let ((node (gethash name *target-info*)))
    (if (not node)
	(progn
	  (setq node (list nil nil nil))
	  (setf (gethash name *target-info*) node)))
    node))

(defun make-out-of-date (target)
  (declare (type string target))
  (let ((node (add-target target)))
    (setf (caddr node) t)  ; it is known
    (setf (cadr node) t))) ; to be out of date

(defun make-not-out-of-date (target)
  (declare (type string target))
  (let ((node (add-target target)))
    (setf (caddr node) t)))

(defun get-current-file-time (name)
  (let* ((cwd (concatenate 'string (sb-posix:getcwd) "/"))
	 (path (concatenate 'string cwd name)))
    (if (probe-file path) 
	(sb-posix:stat-mtime (sb-posix:stat path))
	0)))

(defun get-target-node (name force)
  "Adds name to *target-info* if it's not already registered
   Updates the target time info if unknown or force
   Returns the node
"
  (declare (type string name)
	   (type boolean force))
  (let ((node (add-target name)))
    (if (or force 
	    (null (car node))) ; no stored date
	(setf (car node)
	      (get-current-file-time name)))
    node))

(defun get-file-date (name force)
  "Adds name to *target-info* if it's not already registered
   Updates the file time info if unknown or force
   Returns the file time
"
  (car (get-target-node name force)))

(defun target-rebuild-p (target)
  "Return t if the target is set to be rebuilt"
  (declare (type string target))
  (cadr (get-target-node target nil)))

(defun target-rebuild-known (target)
  "Return t if it is known (already been checked) if a target needs to be built"
  (declare (type string target))
  (caddr (get-target-node target nil)))

(defun is-out-of-date-atom (target dep force)
  "Both target and dep are string file names.
   Return t if target is out-of-date.
"
  (declare (type string target dep)
	   (type boolean force))
  (let* ((tn (get-target-node target force))
	 (dn (get-target-node dep force))
	 (td (car tn))
	 (dd (car dn)))
    (or (cadr tn)
	(cadr dn)
	(zerop td)
	(zerop dd)
	(< td dd))))

(defun is-out-of-date (target dep force)
  "target is an atom.  dep can be an atom or list.
   Returns t if the target is out-of-date with respect to any of the dependencies.
"
  (declare (type string target)
	   (type boolean force))
  (cond ((consp dep)
	 (loop for d in dep
	    do (if (is-out-of-date-atom target d force)
		   (return-from is-out-of-date t)))
	 nil)
	(dep (is-out-of-date-atom target dep force))))

(defmacro not-out-of-date (target dep force)
  `(null (is-out-of-date ,target ,dep ,force)))

(defmacro get-value (v)
  "Returns the value of v except that if the car of the list is a string, the list is assumed to be a list of starting and auto-quoted"
  (let ((v* (gensym)))
    `(let ((,v* ',v))
       (if (atom ,v*)
	   (eval ,v*)
	   (if (stringp (car ,v*))
	       ,v*
	       (eval ,v*))))))

(defun tconc (lst itm)
  "Manage structure to be able to add to the end of a list ala Interlisp tconc.
   This allows lisp to append to the end of arbitrary sized lists in little 
   more time than adding to the beginning of a list.
   An empty list is represented as '(nil) but nil can be passed too (but the
   return value must be saved).  Otherwise lst should be initialized to (list nil).
   lst is the tconc list.
   Use (car lst) to get the actual list from a tconc list.
   itm is the item being added to the end of the list."
  (declare (type list lst))
  (let ((new (list itm)))
    (cond ((null lst)
	   (setq lst (list new))
	   (setf (cdr lst) new))
	  ((null (car lst))
	   (setf (car lst) new)
	   (setf (cdr lst) new))
	  (t 
	   (setf (cddr lst) new)
	   (setf (cdr lst) new)))
    lst))

(defmacro depends (target dependencies &rest recipe)
  "User level function to define dependencies (c-like)
   Usage:  (depends  target(s)  dependency(s)  recipe code ... )
   target and dependencies may be a string or a list of strings
"
  (let ((-target- (gensym))
	(-dependencies- (gensym))
	(-recipe- (gensym)))
    (break)
    `(let ((,-target- (makelist (get-value ,target)))
	   (,-dependencies- (makelist (get-value ,dependencies)))
	   (,-recipe- ',recipe))
       (if (and (null *main-targets*) (null *build-clauses*))
	   (setq *main-targets* ,-target-))
       (setq *build-clauses* (cons (list 'dependency ,-target- ,-dependencies-
					 (if (consp ,-recipe-) 
					     (function (lambda (target dependencies out-of-date-dependencies) ,@recipe))))
				   *build-clauses*))
       (mapc #'add-target ,-target-)
       (mapc #'add-target ,-dependencies-))))

(defmacro corresponding-depends (target-name target dependencies other-dependencies &rest recipe)
  "User level function to define corresponding dependencies (java-like)
  Usage:  (corresponding-depends target-name (target-root-path target-type)  (dependency-root-path  dependency-type)  other-dependencies  recipe-code ...)
"
  (declare (type cons target dependencies))
  (let ((-target- (gensym))
	(-dependencies- (gensym))
	(-other-dependencies- (gensym))
	(-recipe- (gensym)))
    `(let ((,-target- (get-value ,target))
	   (,-dependencies- (get-value ,dependencies))
	   (,-other-dependencies- (get-value ,other-dependencies))
	   (,-recipe- ',recipe))
       (if (and (null *main-targets*) (null *build-clauses*))
	   (setq *main-targets* (list ,target-name)))
       (setq ,-target- (cons (ensure-slash-end (car ,-target-))
			     (cdr ,-target-)))
       (setq ,-dependencies- (cons (ensure-slash-end (car ,-dependencies-))
				   (cdr ,-dependencies-)))
       (setq *build-clauses* (cons (list 'corresponding ,target-name ,-target- ,-dependencies- (makelist ,-other-dependencies-)
					 (if (consp ,-recipe-) 
					     (function (lambda (dependency-root target-root source-file-list source-list-file-name) ,@recipe))))
				   *build-clauses*)))))

(defun main-targets (target)
  "User level function to define the main target"
  (declare (type string target))
  (setq *main-targets* (makelist target)))

(defun string-found (str lst)
  "return t if str found in lst (which may be an atom or list)"
  (declare (type string str))
  (if (atom lst)
      (equal str lst)
      (map nil
	   #'(lambda (s)
	       (if (equal s str)
		   (return-from string-found t)))
	   lst)))

(defun join-set (lst1 lst2)
  (union (makelist lst1) (makelist lst2)))

(defun check-single-target (target)
  (declare (type string target))
  (if (null (target-rebuild-p target))
      (loop for clause in *build-clauses*
	 do (if (is-corresponding clause)
		(cond ((string-found target (c-target clause))
		       (loop for dep in (c-other-deps clause)
			  do (check-single-target dep)
			    (cond ((target-rebuild-p dep)
				   (make-out-of-date target)
				   (return-from check-single-target))))))
		(cond ((string-found target (d-target clause))
		       (loop for dep in (d-deps clause)
			  do (check-single-target dep)
			    (cond ((is-out-of-date target dep nil)
				   (make-out-of-date target)
				   (return-from check-single-target))))))))))

(defun check-multiple-targets (targets)
  (declare (type cons targets))
  (loop for target in targets
     do (check-single-target target))
  t)

(defun sort-single-dependency (target)
  "Go through *build-clauses* and create a like-structured list that includes only what needs to be done and in the correct order for a single target.
   Also returned is a list of the out-of-date dependencies of the target.
"
  (declare (type string target))
  (let (build-clauses dependencies)
					; first the ones that have recipies
    (map nil 
	 #'(lambda (bc)
	     (if (is-dependency bc)
		 (cond ((and (d-recipe bc)                                 ; has recipe
			     (string-found target (d-target bc))           ; applicable target
			     (or (null (d-deps bc))                        ; no dependencies
				 (is-out-of-date target (d-deps bc) nil))) ; has dependencies and some are out-of-date
			(setq dependencies (join-set dependencies (d-deps bc)))
			(setq build-clauses (cons bc build-clauses))))
		 (let ((ti (c-target-info bc))
		       (di (c-dep-info bc)))
		   (cond ((and (equal target (c-target bc))                ; corresponding builds
			       (or (target-rebuild-p (c-target bc))
				   (corresponding-needs-to-be-built (c-type di) (c-root-path di) (c-type ti) (c-root-path ti) (c-other-deps bc))))
			  (setq dependencies (join-set dependencies (c-other-deps bc)))
			  (setq build-clauses (cons bc build-clauses)))))))
	 *build-clauses*)
					; now the ones that have no recipies
    (map nil 
	 #'(lambda (bc)
	     (if (is-dependency bc)
		 (cond ((and (null (d-recipe bc))                          ; doesn't have recipe
			     (string-found target (d-target bc))           ; applicable target
			     (or (null (d-deps bc))                        ; no dependencies
				 (is-out-of-date target (d-deps bc) nil))) ; has dependencies and some are out-of-date
			(setq dependencies (join-set dependencies (d-deps bc)))
			(setq build-clauses (cons bc build-clauses))))))
	 *build-clauses*)
    (values build-clauses dependencies)))

(defun sort-build-clauses (targets)
  (declare (type list targets))
  (let (all-build-clauses)
    (loop for target in targets
       do (multiple-value-bind (build-clauses deps) (sort-single-dependency target)
	    (cond (build-clauses
		   (setq all-build-clauses (append build-clauses all-build-clauses))
		   (loop for d in deps
		      do (if (target-rebuild-p d)	       ; (is-out-of-date target d nil)
			     (setq all-build-clauses (append (sort-build-clauses (list d)) all-build-clauses))))))))
    all-build-clauses))

(defun makelist (x)
  "Takes nil, atom, or list and returns a list.  Good for map."
  (if x
      (if (atom x) 
	  (list x) 
	  x)))

(defun print-list (lst)
  (loop for str in lst
       do (format t "~a " str))
  (terpri))

(defun flatten (list)
  (labels ((flatten2 (list tail)
             (cond
               ((null list) tail)
               ((atom list) (list* list tail))
               (t (flatten2 (first list)
                            (flatten2 (rest list) tail))))))
    (flatten2 list nil)))

(defun run (pgm &rest args)
  (if *verbose*
      (print-list (cons pgm (flatten args))))
  (let* ((res (run-program pgm (flatten args) :search :wait :output *verbose*))
	 (rc (sb-impl::process-%exit-code res)))
    (cond ((/= rc 0)
	   (format t "Process error; aborting build.~%")
	   (error "Run command failed.")))))

(defun print-hash-table (ht)
  "Print the contents of a hashtable"
  (loop for key being the hash-keys of ht
       using (hash-value value)
       do (format t "~S : ~S~%" key value)))

(defun update-file-times ()
  (let ((cwd (concatenate 'string (sb-posix:getcwd) "/")))
    (maphash #'(lambda (key val)
		 (let ((path (concatenate 'string cwd key)))
		   (setf (gethash key *target-info*) 
			 (cons (if (probe-file path) 
				   (sb-posix:stat-mtime (sb-posix:stat key))
				   0) 
			       (cdr val)))))
	     *target-info*)))

(defun create-names (path suffix &rest files)
    "User level function to add a path and suffix to an arbitrary list of file names"
  (loop for file in files
       collect (concatenate 'string path file suffix)))

(defun create-colon-names (path suffix &rest files)
    "User level function to att a path and suffix to an arbitrary list of file names"
    (let ((res ""))
      (loop for file in files
	 do (setq res (concatenate 'string res ":" path file suffix)))
      res))

(defmacro list-names-with-colon (path suffix)
  "User level function to add a path and suffix to an arbitrary list of file names
   path can be nil, a string, or a list of strings
"
  (let ((-path- (gensym))
	(-suffix- (gensym)))
    `(let ((,-path- (get-value ,path))
	   (,-suffix- (get-value ,suffix)))
       (if ,-path-
		 (let ((res "")
		       (paths (makelist ,-path-)))
		   (loop for single-path in paths
		      do (loop for file in (directory (concatenate 'string (setq single-path (ensure-slash-end single-path)) "*" ,-suffix-))
			    do (setq res (concatenate 'string res ":" single-path (file-namestring file)))))
		   res)
		 ""))))

(defmacro create-maven-path (&rest paths)
  (let ((-paths- (gensym)))
    `(let ((,-paths- (get-value ',paths))
	   (mvn-dir (concatenate 'string (namestring (user-homedir-pathname)) ".m2/repository/"))
	   res)
       (loop for path in ,-paths-
	    do (setq res (cons (concatenate 'string mvn-dir path) res)))
       res)))

(defun load-build-file ()
  "Load build.lisp
   Return t if successful
   Return nil if not successful"
  (let ((r (handler-case
	       (progn
		 (handler-bind ((style-warning #'muffle-warning))
		     (load "build.lisp"))
		 t)
	     (t (c)
	       (format *error-output* "~%Error loading file build.lisp~%")
	       (format *error-output* "~a" c)
	       nil))))
    (cond (r
	   (if *verbose*
	       (format t "build.lisp has been loaded~%"))
	   (if (and *verbose* *main-targets*)
	       (progn
		 (format t "Building ")
		 (print-list *main-targets*)))))
    r))

(defun make-ood-dependencies (targets dependencies)
  "Make out-of-date dependencies."
  (let (ood-dependencies)
    (loop for target in (makelist targets)
       do (loop for dep in (makelist dependencies)
	     do (if (is-out-of-date-atom target dep nil)
		    (setq ood-dependencies (cons dep ood-dependencies)))))
    ood-dependencies))

(defun execute-build (clauses)
  "Execute the clauses in order."
  (declare (type list clauses))
  (loop for bc in clauses
     do (if (is-dependency bc) 
	    (if (d-has-recipe bc)
		(handler-case 
		    (funcall (d-recipe bc)
			     (d-target bc)
			     (d-deps bc)
			     (make-ood-dependencies (d-target bc) (d-deps bc)))
		  (t (condition)
		    (format *error-output* "~%Error loading file build.lisp~%")
		    (format *error-output* "~a" condition)
		    (return-from execute-build nil))))
	    (if (c-has-recipe bc)
		(let* ((ti (c-target-info bc))
		       (di (c-dep-info bc))
		       (file-list (get-newer-file-tree (c-type di) (c-root-path di) (c-type ti) (c-root-path ti)))
		       (input-file-name (create-temp-file file-list)))
		  (handler-case
		      (progn
			(funcall (c-recipe bc) ; bc
				 (c-root-path di) ; dependency root directory
				 (c-root-path ti) ; target root directory
				 file-list ; list of source files that need to be built
				 input-file-name ; name of temp file hold list of files that need to be built
				 )
			(delete-file input-file-name))
		    (t (condition)
		      (format *error-output* "~%Error loading file build.lisp~%")
		      (format *error-output* "~a" c)
		      (return-from execute-build nil)))))))
  t)

(defun absolute-path (path)
  "Takes a relative path and returns an absolute path"
  (cond ((eql (aref path 0) #\/)
	 path)
	((and (= 1 (length path))
	      (eql (aref path 0) #\.))
	 (concatenate 'string (getcwd) "/"))
	((and (= 2 (length path))
	      (string= path "./"))
	 (concatenate 'string (getcwd) "/"))
	((and (< 2 (length path))
	      (eql (aref path 0) #\.)
	      (eql (aref path 1) #\/))
	 (concatenate 'string (getcwd) path))
	(t
	 (concatenate 'string (getcwd) "/" path))))

(defun all-entries-in-current-directory ()
  "Returns all the files and directories in the current directory"
  (loop for ent in (directory "*.*")
     collect (let ((fn (file-namestring ent)))
	       (if (zerop (length fn))
		   (enough-namestring ent)
		   fn))))

(defun ensure-slash-end (name)
  (declare (type string name))
  (if (eql #\/ (char name (1- (length name))))
      name
      (concatenate 'string name "/")))

(defun get-name-type (name)
  "Return the type of file name is:
      0 = non-existant
      1 = file
      2 = directory
      3 = other
"
  (declare (type string name))
  (handler-case
      (progn
	(let* ((sb (sb-posix:stat name))
	       (mode (sb-posix:stat-mode sb)))
	  (cond ((plusp (logand mode #o100000)) 1)
		((plusp (logand mode #o40000)) 2)
		(t 3))))
    (t (e) 0)))

(defun string-ends-with-p (str1 str2)
  "Determine whether `str1` ends with `str2`"
  (declare (type string str1 str2))
  (let ((p (mismatch str2 str1 :from-end T)))
    (or (not p) (= 0 p))))

(defun get-newer-file-tree (dep-type dep-root-dir target-type target-root-path &optional path)
  "Returns a list of files that exist within an entire directory tree with a given file extension that are newer than corresponding files indicated by target-type target-root-path
   Example:  (get-newer-file-tree \".java\" \"src\" \".class\" \"out/\")
"
  (declare (type string dep-type dep-root-dir target-type target-root-path))
  (if (not path)
      (setq path dep-root-dir))
  (let (res)
    (loop for ent in (directory (concatenate 'string (ensure-slash-end path) "*.*"))
       do (let ((ents (enough-namestring ent))
		(src-path (enough-namestring (directory-namestring ent))))
	    (if (or (not (search "/." ents))
		    (not (eql #\. (char ents 0))))
		(case (get-name-type ents)
		  (1			; file
		   (if (and (string-ends-with-p ents dep-type)
			    (let ((new-file (get-corresponding-file dep-root-dir ents dep-type target-root-path target-type)))
			      (is-out-of-date-atom new-file ents nil)))
		       (setq res (cons ents res))))
		  (2			; directory
		   (let ((res2 (get-newer-file-tree dep-type dep-root-dir target-type target-root-path ents)))
		     (if res2
			 (setq res (append res res2)))))))))
    res))

(defun corresponding-needs-to-be-built (dep-type dep-root-dir target-type target-root-path library-files &optional path)
  "Returns t if clause needs to be run, nil otherwise
   Example:  (corresponding-needs-to-be-built \".java\" \"src\" \".class\" \"out/\" library-files)
"
  (declare (type string dep-type dep-root-dir target-type target-root-path))
  (if (not path)
      (setq path dep-root-dir))
  (loop for ent in (directory (concatenate 'string (ensure-slash-end path) "*.*"))
     do (let ((ents (enough-namestring ent))
	      (src-path (enough-namestring (directory-namestring ent))))
	  (if (or (not (search "/." ents))
		  (not (eql #\. (char ents 0))))
	      (case (get-name-type ents)
		(1			; file
		 (if (and (string-ends-with-p ents dep-type)
			  (not (equal "package-info.java" (file-namestring ent))) ; class files don't get built for the file
			  (let ((new-file (get-corresponding-file dep-root-dir ents dep-type target-root-path target-type)))
			    (or (is-out-of-date-atom new-file ents nil)
				(is-out-of-date new-file library-files nil))))
		     (return-from corresponding-needs-to-be-built t)))
		(2			; directory
		 (let ((res2 (corresponding-needs-to-be-built dep-type dep-root-dir target-type target-root-path library-files ents)))
		   (if res2
		       (return-from corresponding-needs-to-be-built t))))))))
  nil)

(defun get-file-tree (type path)
  "Returns a list of files that exist within an entire directory tree with a given file extension.
   If type is \"\", get all files.
   Example:  (get-file-tree \".java\" \"src\")
"
  (declare (type string type path))
  (let (res)
    (loop for ent in (directory (concatenate 'string (ensure-slash-end path) "*.*"))
       do (let ((ents (enough-namestring ent)))
	    (if (or (not (search "/." ents))
		     (not (eql #\. (char ents 0))))
		(case (get-name-type ents)
		  (1			; file
		   (if (string-ends-with-p ents type)
		       (setq res (cons ents res))))
		  (2			; directory
		   (let ((res2 (get-file-tree type ents)))
		     (if res2
			 (setq res (append res res2)))))))))
    res))

(defun get-corresponding-file (src-root-dir src-full-path src-extension target-root-dir target-extension)
  (declare (type string src-root-dir src-full-path src-extension target-root-dir target-extension))
  (concatenate 'string
	       (ensure-slash-end target-root-dir)
	       (subseq src-full-path (length (ensure-slash-end src-root-dir)) (- (length src-full-path) (length src-extension)))
	       target-extension))

(defun create-temp-file (lst)
  "Create a temporary file with the lst of strings.  Return the file name."
  (let ((fname (format nil "~0d.tmp" (random 1000000 (make-random-state t)))))
    (with-open-file (stream fname :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (loop for file in lst
	   do (format stream "~a~%" file)))
    fname))

(defun reset-target-info ()
  (clrhash *target-info*))

(defun rm (&rest file-list)
  "User-level function to remove files"
  (loop for file-spec in file-list
     do (loop for file in (directory file-spec)
	   do (if *verbose*
		  (format t "rm ~a~%" (enough-namestring file)))
	     (delete-file file))))

(defun rmdir (&rest directory-list)
  "User-level function to remove directories"
  (loop for dir-spec in directory-list
     do (loop for dir in (directory dir-spec)
	   do (if *verbose*
		  (format t "rmdir ~a~%" (enough-namestring dir)))
	     (sb-ext:delete-directory dir :recursive t))))

(defun getcwd ()
  "User-level function to get the current working directory."
  (sb-posix:getcwd))

(defun chdir (path)
  "User-level function to change directory."
  (if *verbose*
      (format t "chdir ~a~%" path))
  (setq *default-pathname-defaults* (truename path))
  (sb-posix:chdir path))

(defun pushd (&optional path)
  "User-level function to change directory and store the previous path on a LIFO stack."
  (cond ((not path)
	 (setq path (getcwd))
	 (popd)
	 (setq *directory-stack* (cons path *directory-stack*)))
	(t
	 (setq *directory-stack* (cons (getcwd) *directory-stack*))
	 (chdir path))))

(defun popd ()
  "User-level function to pop the saved path from the path stack."
  (let ((path (car *directory-stack*)))
    (setq *directory-stack* (cdr *directory-stack*))
    (chdir path)))

;; language specific extensions


(defmacro build-java (name src-path target-path &key lib-path other-deps)
  "User level function for building class files from java source files"
  `(corresponding-depends ,name (,target-path ".class") (,src-path ".java")
			  ,other-deps
			  (ensure-directories-exist target-root)
			 ; (format t "~a~%" source-file-list)
			  (run "javac" "-cp" (concatenate 'string target-root (list-names-with-colon ,lib-path ".jar")) "-sourcepath" dependency-root "-d" target-root (format nil "@~a" source-list-file-name))))

(defmacro jar-depends (jar-with-path  root-path-to-jar &optional dependency)
  "User-level convenience function for building jar files"
  (let ((dep-list
	 (append (makelist dependency) (list (function (lambda ()
						(let (res)
						  (pushd root-path-to-jar)
						  (setq res (all-entries-in-current-directory))
						  (popd)
						  res)))))))
    `(depends ,jar-with-path ,dep-list
	      (let ((full-jar-path (absolute-path (car target))))
		(pushd ,root-path-to-jar)
		(apply #'run (cons "jar" (cons "cvf" (cons full-jar-path 
							   (let (res)
							     (pushd ,root-path-to-jar)
							     (setq res (all-entries-in-current-directory))
							     (popd)
							     res)))))
		(popd)))))

(defmacro build-children (&rest dirs)
  "Recipe function used to build a series of child projects"
  (let ((-dirs- (gensym)))
    `(let ((,-dirs- ',dirs))
       (loop for dir in ,-dirs-
	  do (pushd dir)
	    (run *build* *build-args*)
	    (popd)))))

(defun repl ()
  (format t "~%Use :exit to quit~%~%")
  (loop
     (format t "BUILD> ")
     (force-output)
     (let ((expr (read)))
       (cond ((eq expr :exit)
	      (return))
	     (t
	      (format t "~s~%" (eval expr))
	      (force-output))))))

;; end

(defun build (&rest args)
  (if *debugging*
      (progn (reset-target-info)
	     (setq *build-clauses* nil)
	     (setq *main-targets* nil)
	     (setq *build* nil)
	     (setq *build-args* args))
      (progn
	(setq *build* (namestring (truename (car sb-ext:*posix-argv*))))
	(setq *build-args* (cdr sb-ext:*posix-argv*))))
  (if *build-args*
      (setq *main-targets* *build-args*))
  
  
  (load-build-file)
  (break)
  
  
  (if (load-build-file)
      (cond (*main-targets*
	     (check-multiple-targets *main-targets*)
	     (let ((build-clauses (sort-build-clauses *main-targets*)))
	       (if (consp build-clauses)
		   (execute-build build-clauses)
		   (progn (format t "Nothing to do for ")
			  (print-list *main-targets*)))))
	    (t (format t "No main target to build~%")))))

(if (not *debugging*)
    (save-lisp-and-die "build" :executable t :toplevel #'build))



