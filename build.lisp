
; build.lisp
; written by Blake McBride

(defparameter *debugging* nil)

(require :sb-posix)

; muffle all compiler warnings
(declaim (sb-ext:muffle-conditions cl:warning))

;;(sb-posix:chdir "/home/blake/Build")
(if *debugging*
    (setq *default-pathname-defaults* #P"/home/blake/Build/"))

;(require "asdf")
;(require :trivial-features)
;(require :alexandria)
;(require :babel)
;(require :cffi)

; dependency
; (target-files dependent-files)

(defparameter *verbose* t
  "verbose output of commands")

(defparameter *file-info* (make-hash-table :test 'equal :size 100000)
  "full-file-name - (file-date out-of-date)")

(defparameter *build-clauses* nil
     "list of dependencies
      Each element looks like: (target dependencies code-to-create ... )
")

(defparameter *main-targets* nil
  "This is what is being built.  List of string targets.")

(defun add-file (name)
  "Adds name to *file-info* if it's not already registered
   Returns the name-specific node
"
  (let ((node (gethash name *file-info*)))
    (if (not node)
	(progn
	  (setq node '(nil nil))
	  (setf (gethash name *file-info*) node)))
    node))

(defun get-current-file-time (name)
  (let* ((cwd (concatenate 'string (sb-posix:getcwd) "/"))
	 (path (concatenate 'string cwd name)))
    (if (probe-file path) 
	(sb-posix:stat-mtime (sb-posix:stat path))
	0)))

(defun get-file-date (name)
  "Adds name to *file-info* if it's not already registered
   Updates the file time info
   Returns the file time
"
  (let ((node (add-file name)))
    (if (null (car node))
	(setf (car node)
	      (get-current-file-time name)))
    (car node)))

(defun is-out-of-date-atom (target dep)
  "Both target and dep are string file names.
   Return t if target is out-of-date.
"
  (let ((td (get-file-date target))
	(dd (get-file-date dep)))
    (or (zerop td)
	(< td dd))))

(defun is-out-of-date (target dep)
  "target is an atom.  dep can be an atom or list.
   Returns t if the target is out-of-date with respect to any of the dependencies.
"
  (block ood
    (cond ((consp dep)
	   (loop for d in dep
	      do (if (is-out-of-date-atom target d)
		     (return-from ood t)))
	   nil)
	  (t (is-out-of-date-atom target dep)))))

(defmacro not-out-of-date (target dep)
  `(null (is-out-of-date ,target ,dep)))

(defun get-value (v)
  "Returns the value of v except that if the car of the list is a string, the list is assumed to be a list of starting and auto-quoted"
  (if (atom v)
      (eval v)
      (if (stringp (car v))
	  v
	  (eval v))))

(defun tconc (lst itm)
  "Manage structure to be able to add to the end of a list ala Interlisp tconc.
   This allows lisp to append to the end of arbitrary sized lists in little 
   more time than adding to the beginning of a list.
   An empty list is represented as '(nil) but nil can be passed too (but the
   return value must be saved).
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
  "User level function to define dependencies"
  (let ((-target- (gensym))
	(-dependencies- (gensym)))
    `(let ((,-target- (makelist (get-value ,target)))
	   (,-dependencies- (makelist (get-value ,dependencies))))
       (progn 
	 (if (and (null *main-targets*) (null *build-clauses*))
	     (setq *main-targets* ,-target-))
	 (setq *build-clauses* (cons (list ,-target- ,-dependencies-
	 				  (if (listp ',recipe) 
	 				      (function (lambda (target dependencies) ,@recipe)))) 
	 			    *build-clauses*))
	 (mapc #'add-file ,-target-)
	 (mapc #'add-file ,-dependencies-)))))

(defun main-targets (target)
  "User level function to define the main target"
  (declare (type string target))
  (setq *main-targets* (makelist target)))

(defun string-found (str lst)
  "return t if str found in lst (which may be an atom or list)"
  (declare (type string str))
  (block sf
    (if (atom lst)
	(equal str lst)
	(map nil 
	     #'(lambda (s)
		     (if (equal s str)
			 (return-from sf t)))
	     lst))))

(defun join-set (lst1 lst2)
  (union (makelist lst1) (makelist lst2)))

(defun sort-single-dependency (target)
  "Go through *build-clauses* and create a like-structured list that includes only what needs to be done and in the correct order for a single target
   Also returned is a list of the out-of-date dependencies of the target.
"
  (declare (type string target))
  (let (deps  alldeps)
					; first the ones that have receipies
    (map nil 
	 #'(lambda (d)
	     (format t "d = ~%~s~%" d)
             (format t "target = ~s~%" target)
	     (format t "(string-found target (car d)) = ~s~%" (string-found target (car d)))
	     (format t "(is-out-of-date target (cadr d)) = ~s~%" (is-out-of-date target (cadr d)))
	     (cond ((and (cddr d)	                        ; has receipe
			 (string-found target (car d))          ; applicable target
			 (or (null (cadr d))                    ; no dependencies
			     (is-out-of-date target (cadr d)))) ; has dependencies and some are out-of-date
		    (format t "test passed~%")
		    (setq alldeps (join-set alldeps (cadr d)))
		    (setq deps (cons d deps)))))
	 *build-clauses*)
					; now the ones that have no receipies
    (map nil 
	 #'(lambda (d)
	     (cond ((and (null (cddr d))                        ; doesn't have receipe
			 (string-found target (car d))          ; applicable target
			 (or (null (cadr d))                    ; no dependencies
			     (is-out-of-date target (cadr d)))) ; has dependencies and some are out-of-date  
		    (setq alldeps (join-set alldeps (cadr d)))
		    (setq deps (cons d deps)))))
	 *build-clauses*)
    (values deps alldeps)))

(defun sort-build-clauses (targets)
  (declare (type list targets))
  (let (allcmds)
    (loop for target in targets
       do (multiple-value-bind (cmds deps) (sort-single-dependency target)
	    (cond (cmds
		   (setq allcmds (append cmds allcmds))
		   (loop for d in deps
		      do (if (is-out-of-date target d)
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

(defun run (pgm &rest args)
  (if *verbose*
      (print-list (cons pgm args)))
  (run-program pgm args :search :wait :output *verbose*))


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
	      (print-list *main-targets*)
	      (terpri)))
	t)
    (t (c)
      (format *error-output* "~%Error loading file build.build~%")
      nil)))

(defun execute-build (cmds)
  (loop for cmd in cmds
     do (let ((res (funcall (caddr cmd)
			    (caar cmd)
			    (caadr cmd))))
	  (setq res (sb-impl::process-%exit-code res))
	  (if (/= res 0)
	      (progn (format t "Process error; aborting build.~%")
		     (return-from execute-build nil)))))
  t)

(defun main ()
  (if *debugging*
      (progn (clrhash *file-info*)
	     (setq *build-clauses* nil)
	     (setq *main-targets* nil)))
  (if (cdr sb-ext:*posix-argv*)
      (setq *main-targets* (cdr sb-ext:*posix-argv*)))
  (and (load-build-file)
       (progn
	 (if *debugging*
	     (progn
	       (format t "*build-clauses* = ~%~a~%" *build-clauses*)
	       (format t "sorted build clauses = ~%~s~%" (sort-build-clauses *main-targets*))))
	(execute-build (sort-build-clauses *main-targets*)))))

(if (not *debugging*)
    (save-lisp-and-die "build" :executable t :toplevel #'main))
