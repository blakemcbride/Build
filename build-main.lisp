
; build.lisp
; written by Blake McBride

(defparameter *debugging* nil)

(require :sb-posix)

; muffle all compiler warnings
(declaim (sb-ext:muffle-conditions cl:warning))

(if *debugging* (declaim (optimize (debug 3))))

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

(defparameter *file-info* (make-hash-table :test 'equal :size 100000)
  "full-file-name - (file-date out-of-date)")

(defparameter *build-clauses* nil
     "list of dependencies
      Each element looks like: 
         (target dependencies 
                 code-to-create ... )
             or
         ('corresponding target-name (target-root-path target-type) (dependency-root-path dependency-type)
                 code-to-create ... )
")

(defun is-corresponding-tag (tag)
  "Return t if tag is a tag for a correponding dependency"
  (loop for clause in *build-clauses*
     do (if (and (eq 'corresponding (car clause))
		 (string= tag (cadr clause)))
	    (return-from is-corresponding-tag t)))
  nil)

(defparameter *directory-stack* nil)

(defparameter *main-targets* nil
  "This is what is being built.  List of string targets.")

(defmacro caddddr (x)
  `(car (cddddr ,x)))

(defmacro cadaddr (x)
  `(car (cdaddr ,x)))

(defun add-file (name)
  "Adds name to *file-info* if it's not already registered
   Returns the name-specific node
"
  (declare (type string name))
  (let ((node (gethash name *file-info*)))
    (if (not node)
	(progn
	  (setq node (list nil nil))
	  (setf (gethash name *file-info*) node)))
    node))

(defun make-out-of-date (file)
  (declare (type string file))
  (let ((node (add-file file)))
    (setf (cadr node) t)))

(defun get-current-file-time (name)
  (let* ((cwd (concatenate 'string (sb-posix:getcwd) "/"))
	 (path (concatenate 'string cwd name)))
    (if (probe-file path) 
	(sb-posix:stat-mtime (sb-posix:stat path))
	0)))

(defun get-file-node (name force)
  "Adds name to *file-info* if it's not already registered
   Updates the file time info if unknown or force
   Returns the node
"
  (declare (type string name)
	   (type boolean force))
  (let ((node (add-file name)))
    (if (or force 
	    (null (car node))) ; no stored date
	(setf (car node)
	      (get-current-file-time name)))
    node))

(defun get-file-date (name force)
  "Adds name to *file-info* if it's not already registered
   Updates the file time info if unknown or force
   Returns the file time
"
  (car (get-file-node name force)))

(defun file-needs-to-be-rebuilt (file)
  (declare (type string file))
  (cadr (get-file-node file nil)))

(defun is-out-of-date-atom (target dep force)
  "Both target and dep are string file names.
   Return t if target is out-of-date.
"
  (declare (type string target dep)
	   (type boolean force))
  (let* ((tn (get-file-node target force))
	 (dn (get-file-node dep force))
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
  (block ood
    (cond ((consp dep)
	   (loop for d in dep
	      do (if (is-out-of-date-atom target d force)
		     (return-from ood t)))
	   nil)
	  (t (is-out-of-date-atom target dep force)))))

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
   target and dpendencies may be a string or a list of strings
"
  (let ((-target- (gensym))
	(-dependencies- (gensym))
	(-recipe- (gensym)))
    `(let ((,-target- (makelist (get-value ,target)))
	   (,-dependencies- (makelist (get-value ,dependencies)))
	   (,-recipe- ',recipe))
       (if (and (null *main-targets*) (null *build-clauses*))
	   (setq *main-targets* ,-target-))
       (setq *build-clauses* (cons (list ,-target- ,-dependencies-
					 (if (consp ,-recipe-) 
					     (function (lambda (target dependencies out-of-date-dependencies) ,@recipe))))
				   *build-clauses*))
       (mapc #'add-file ,-target-)
       (mapc #'add-file ,-dependencies-))))

(defmacro corresponding-depends (target-name target dependencies &rest recipe)
  "User level function to define corresponding dependencies (java-like)
  Usage:  (corresponding-depends target-name (target-root-path target-type)  (dependency-root-path  dependency-type)    recipe code ...)
"
  (declare (type cons target dependencies))
  (let ((-target- (gensym))
	(-dependencies- (gensym))
	(-recipe- (gensym)))
    `(let ((,-target- (get-value ,target))
	   (,-dependencies- (get-value ,dependencies))
	   (,-recipe- ',recipe))
       (if (and (null *main-targets*) (null *build-clauses*))
	   (setq *main-targets* (list ,target-name)))
       (setq ,-target- (cons (ensure-slash-end (car ,-target-))
			     (cdr ,-target-)))
       (setq ,-dependencies- (cons (ensure-slash-end (car ,-dependencies-))
				   (cdr ,-dependencies-)))
       (setq *build-clauses* (cons (list 'corresponding ,target-name ,-target- ,-dependencies-
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
  (if (null (file-needs-to-be-rebuilt target))
      (loop for clause in *build-clauses*
	 do (if (not (eq 'corresponding (car clause)))
		(cond ((string-found target (car clause))
		       (loop for dep in (cadr clause)
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
  "Go through *build-clauses* and create a like-structured list that includes only what needs to be done and in the correct order for a single target
   Also returned is a list of the out-of-date dependencies of the target.
"
  (declare (type string target))
  (let (build-clauses dependencies)
					; first the ones that have recipies
    (map nil 
	 #'(lambda (bc)
	     (if (not (eq 'corresponding (car bc)))
		 (cond ((and (cddr bc)	                                 ; has recipe
			     (string-found target (car bc))              ; applicable target
			     (or (null (cadr bc))                        ; no dependencies
				 (is-out-of-date target (cadr bc) nil))) ; has dependencies and some are out-of-date
			(setq dependencies (join-set dependencies (cadr bc)))
			(setq build-clauses (cons bc build-clauses))))
		 (let ((ti (caddr bc))
		       (di (cadddr bc)))
		   (if (and  (equal target (cadr bc))                    ; corresponding builds
			     (or (file-needs-to-be-rebuilt (cadr bc))
				 (corresponding-needs-to-be-built (cadr di) (car di) (cadr ti) (car ti))))
		       (setq build-clauses (cons bc build-clauses))))))
	 *build-clauses*)
					; now the ones that have no recipies
    (map nil 
	 #'(lambda (bc)
	     (if (not (eq 'corresponding (car bc)))
		 (cond ((and (null (cddr bc))                            ; doesn't have recipe
			     (string-found target (car bc))              ; applicable target
			     (or (null (cadr bc))                        ; no dependencies
				 (is-out-of-date target (cadr bc) nil))) ; has dependencies and some are out-of-date
			(setq dependencies (join-set dependencies (cadr bc)))
			(setq build-clauses (cons bc build-clauses))))))
	 *build-clauses*)
    (values build-clauses dependencies)))

(defun sort-build-clauses (targets)
  (declare (type list targets))
  (let (allcmds)
    (loop for target in targets
       do (multiple-value-bind (cmds deps) (sort-single-dependency target)
	    (cond (cmds
		   (setq allcmds (append cmds allcmds))
		   (loop for d in deps
		      do (if (is-out-of-date target d nil)
			     (setq allcmds (append (sort-build-clauses (list d)) allcmds))))))))
    allcmds))

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
		   (setf (gethash key *file-info*) 
			 (cons (if (probe-file path) 
				   (sb-posix:stat-mtime (sb-posix:stat key))
				   0) 
			       (cdr val)))))
	     *file-info*)))

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
  (handler-case
      (progn
	(load "build.lisp")
	(if *verbose*
	    (format t "build.lisp has been loaded~%"))
	(if (and *verbose* *main-targets*)
	    (progn
	      (format t "Building ")
	      (print-list *main-targets*)))
	t)
    (t (c)
      (format *error-output* "~%Error loading file build.lisp~%")
      (if t (format *error-output* "~s" c))
      nil)))

(defun make-ood-dependencies (targets dependencies)
  (let (ood-dependencies)
    (loop for target in (makelist targets)
       do (loop for dep in (makelist dependencies)
	     do (if (is-out-of-date-atom target dep nil)
		    (setq ood-dependencies (cons dep ood-dependencies)))))
    ood-dependencies))

(defun execute-build (cmds)
  (declare (type list cmds))
  (loop for cmd in cmds
     do (if (not (eq 'corresponding (car cmd))) 
	    (if (caddr cmd)
		(funcall (caddr cmd)
			 (caar cmd)
			 (caadr cmd)
			 (make-ood-dependencies (caar cmd) (caadr cmd))))
	    (if (caddddr cmd)
		(let* ((ti (caddr cmd))
		       (di (cadddr cmd))
		       (file-list (get-newer-file-tree (cadr di) (car di) (cadr ti) (car ti)))
		       (input-file-name (create-temp-file file-list)))
		  (funcall (caddddr cmd)             ; cmd
				     (car di)        ; dependency root directory
				     (car ti)        ; target root directory
				     file-list       ; list of source files that need to be built
				     input-file-name ; name of temp file hold list of files that need to be built
				     )
		  (delete-file input-file-name)))))
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

(defun corresponding-needs-to-be-built (dep-type dep-root-dir target-type target-root-path &optional path)
  "Returns t if clause needs to be run, nil otherwise
   Example:  (corresponding-needs-to-be-built \".java\" \"src\" \".class\" \"out/\")
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
			    (is-out-of-date-atom new-file ents nil)))
		     (return-from corresponding-needs-to-be-built t)))
		(2			; directory
		 (let ((res2 (corresponding-needs-to-be-built dep-type dep-root-dir target-type target-root-path ents)))
		   (if res2
		       (return-from corresponding-needs-to-be-built t))))))))
  nil)

(defun get-file-tree (type path)
  "Returns a list of files that exist within an entire directory tree with a given file extension.
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

(defun reset-file-info ()
  (clrhash *file-info*))

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


(defmacro build-java (name src-path target-path &optional lib-path)
  "User level function for building class files from java source files"
   `(corresponding-depends ,name (,target-path ".class") (,src-path ".java")
        (ensure-directories-exist target-root)
        (run "javac" "-cp" (concatenate 'string target-root (list-names-with-colon ,lib-path ".jar")) "-sourcepath" dependency-root "-d" target-root (format nil "@~a" source-list-file-name))))

(defmacro jar-depends (jar-with-path  root-path-to-jar)
  "User-level convenience function for building jar files"
  (let ((-root-path-to-jar- (gensym))
        (-jar-with-path- (gensym)))
   `(let ((,-root-path-to-jar- ,root-path-to-jar)
          (,-jar-with-path- ,jar-with-path))
       (depends ,-jar-with-path- (get-file-tree ,-root-path-to-jar-)
         (pushd ,-root-path-to-jar-)
         (apply #'run (cons "jar" (cons "cvf" (cons (absolute-path (car target)) (all-entries-in-current-directory)))))
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

(defun main ()
  (if *debugging*
      (progn (reset-file-info)
	     (setq *build-clauses* nil)
	     (setq *main-targets* nil)))
  (setq *build* (namestring (truename (car sb-ext:*posix-argv*))))
  (setq *build-args* (cdr sb-ext:*posix-argv*))
  (if (cdr sb-ext:*posix-argv*)
      (setq *main-targets* (cdr sb-ext:*posix-argv*)))
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
    (save-lisp-and-die "build" :executable t :toplevel #'main))

