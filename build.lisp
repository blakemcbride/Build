
; build.lisp
; written by Blake McBride

(defparameter *debugging* nil)

(require :sb-posix)

; muffle all compiler warnings
(declaim (sb-ext:muffle-conditions cl:warning))

;;(sb-posix:chdir "/home/blake/Build-master")
(if *debugging*
    (setq *default-pathname-defaults* #P"/home/blake/Build-master/"))

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

(defparameter *dependencies* nil
     "list of dependencies")

(defparameter *main-target* nil
  "This is what is being built")

(defun add-file (name)
  "Adds name to *file-info*"
  (if (not (nth-value 1 (gethash name *file-info*)))
      (setf (gethash name *file-info*) '(nil nil))))

(defun get-value (v)
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
	 (if (and (null *main-target*) (null *dependencies*))
	     (setq *main-target* ,-target-))
	 (setq *dependencies* (cons (list ,-target- ,-dependencies-
	 				  (if (listp ',recipe) 
	 				      (function (lambda (target dependencies) ,@recipe)))) 
	 			    *dependencies*))
	 (mapc #'add-file ,-target-)
	 (mapc #'add-file ,-dependencies-)))))

(defun main-target (target)
  "User level function to define the main target"
  (setq *main-target* target))

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

;(format t "~a~%" *dependencies*)

;(format t "~a~%" (caddar *dependencies*))

;(run-program "ls" nil :search :wait :output *verbose*)

;; (print (funcall (caddar *dependencies*) 
;; 		(caar *dependencies*) 
;; 		(cadar *dependencies*)))

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
	(if *main-target*
	    (progn
	      (format t "Building ")
	      (print-list *main-target*)
	      (terpri)))
	t)
    (t (c)
      (format *error-output* "~%Error loading file build.build~%")
      nil)))

(defun execute-build (deps)
  (loop for dep in deps
     do (let ((res (funcall (caddr dep)
			    (caar dep)
			    (caadr dep))))
	  (setq res (sb-impl::process-%exit-code res))
	  (if (/= res 0)
	      (progn (format t "Process error; aborting build.~%")
		     (return-from execute-build nil))
	      )))
  t)

(defun main ()
  (if *debugging*
      (progn (clrhash *file-info*)
	     (setq *dependencies* nil)
	     (setq *main-target* nil)))
  (if (cdr sb-ext:*posix-argv*)
      (setq *main-target* (cadr sb-ext:*posix-argv*)))
  (and (load-build-file)
       (progn
	 (format t "~a~%" *dependencies*)
	 (execute-build *dependencies*))))

  ;; (format t "~s~%" sb-ext:*posix-argv*)

(if (not *debugging*)
    (save-lisp-and-die "build" :executable t :toplevel #'main))
