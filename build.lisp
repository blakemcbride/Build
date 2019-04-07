
; build.lisp
; written by Blake McBride

(defparameter *debugging* nil)

(require :sb-posix)

; muffle all compiler warnings
(declaim (sb-ext:muffle-conditions cl:warning))

(if *debugging* (declaim (optimize (debug 3))))

(if *debugging*
    (sb-posix:chdir "/home/blake/Build"))

;(require "asdf")
;(require :trivial-features)
;(require :alexandria)
;(require :babel)
;(require :cffi)


(defparameter *verbose* t
  "verbose output of commands")

(defparameter *file-info* (make-hash-table :test 'equal :size 100000)
  "full-file-name - (file-date out-of-date)")

(defparameter *build-clauses* nil
     "list of dependencies
      Each element looks like: 
         (target dependencies 
                 code-to-create ... )
             or
         ('corresponding (target-path target-type) (dependency-path dependency-type)
                 code-to-create ... )
")

(defparameter *main-targets* nil
  "This is what is being built.  List of string targets.")

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
  "User level function to define dependencies (c-like)"
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

(defmacro corresponding-depends (target dependencies &rest recipe)
  "User level function to define corresponding dependencies (java-like)"
  (let ((-target- (gensym))
	(-dependencies- (gensym))
	(-recipe- (gensym)))
    `(let ((,-target- (makelist (get-value ,target)))
	   (,-dependencies- (makelist (get-value ,dependencies)))
	   (,-recipe- ',recipe))
       (if (and (null *main-targets*) (null *build-clauses*))
	   (setq *main-targets* ,-target-))
       (setq *build-clauses* (cons (list 'corresponding ,-target- ,-dependencies-
					 (if (consp ,-recipe-) 
					     (function (lambda (target dependencies out-of-date-dependencies) ,@recipe))))
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
	 do (cond ((string-found target (car clause))
		   (loop for dep in (cadr clause)
		      do (check-single-target dep)
			(cond ((is-out-of-date target dep nil)
			       (make-out-of-date target)
			       (return-from check-single-target)))))))))

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
					; first the ones that have receipies
    (map nil 
	 #'(lambda (bc)
	     (cond ((and (cddr bc)	                           ; has receipe
			 (string-found target (car bc))            ; applicable target
			 (or (null (cadr bc))                      ; no dependencies
			     (is-out-of-date target (cadr bc) nil))) ; has dependencies and some are out-of-date
		    (setq dependencies (join-set dependencies (cadr bc)))
		    (setq build-clauses (cons bc build-clauses)))))
	 *build-clauses*)
					; now the ones that have no receipies
    (map nil 
	 #'(lambda (bc)
	     (cond ((and (null (cddr bc))                          ; doesn't have receipe
			 (string-found target (car bc))            ; applicable target
			 (or (null (cadr bc))                      ; no dependencies
			     (is-out-of-date target (cadr bc) nil))) ; has dependencies and some are out-of-date  
		    (setq dependencies (join-set dependencies (cadr bc)))
		    (setq build-clauses (cons bc build-clauses)))))
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
  (run-program pgm (flatten args) :search :wait :output *verbose*))


;; (depends "file.o" "file.c" 
;; 	 (run "gcc" "-c" dependencies)
;; 	 )

;(format t "~a~%" *build-clauses*)

;(format t "~a~%" (caddar *build-clauses*))

;(run-program "ls" nil :search :wait :output *verbose*)

;; (print (funcall (caddar *build-clauses*) 
;; 		(caar *build-clauses*) 
;; 		(cadar *build-clauses*)))

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

(defun load-build-file ()
  "Load build.build
   Return t if successful
   Return nil if not successful"
  (handler-case
      (progn
	(load "build.build")
	(format t "build.build has been loaded~%")
	(if *main-targets*
	    (progn
	      (format t "Building ")
	      (print-list *main-targets*)))
	t)
    (t (c)
      (format *error-output* "~%Error loading file build.build~%")
      nil)))

(defun make-ood-dependencies (targets dependencies)
  (let (ood-dependencies)
    (loop for target in (makelist targets)
       do (loop for dep in (makelist dependencies)
	     do (if (is-out-of-date-atom target dep nil)
		    (setq ood-dependencies (cons dep ood-dependencies)))))
    ood-dependencies))

(defun execute-build (cmds)
  (loop for cmd in cmds
     do (if (caddr cmd)
	    (let ((res (funcall (caddr cmd)
				(caar cmd)
				(caadr cmd)
				(make-ood-dependencies (caar cmd) (caadr cmd)))))
	      (cond (res
		     (setq res (sb-impl::process-%exit-code res))
		     (if (/= res 0)
			 (progn (format t "Process error; aborting build.~%")
				(return-from execute-build nil))))))))
  t)

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

(defun get-file-tree (type path)
  "Returns a list of files that exist within an entire directory tree with a given file extension.
   Example:  (get-file-tree \".java\" \".\")
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

(defun reset-file-info ()
  (clrhash *file-info*))

(defun main ()
  (if *debugging*
      (progn (reset-file-info)
	     (setq *build-clauses* nil)
	     (setq *main-targets* nil)))
  (if (cdr sb-ext:*posix-argv*)
      (setq *main-targets* (cdr sb-ext:*posix-argv*)))
  (and (load-build-file)
       (check-multiple-targets *main-targets*)
       (execute-build (sort-build-clauses *main-targets*))))

(if (not *debugging*)
    (save-lisp-and-die "build" :executable t :toplevel #'main))

