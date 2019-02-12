
(sb-posix:chdir "/home/blake/build")

;(require "asdf")
;(require :trivial-features)
;(require :alexandria)
;(require :babel)
;(require :cffi)

; dependency
; (target-files dependent-files)

(defparameter *verbose* t
  "verbose output of commands")

(defparameter *file-info* (make-hash-table :test 'equal)
  "full-file-name - (file-date out-of-date)")

(defparameter *dependencies* nil
  "list of dependencies")

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for nil in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

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


(defmacro depends (target dependencies &rest recipe)
  (let ((-target- (gensym))
	(-dependencies- (gensym)))
    `(let ((,-target- (get-value ,target))
	   (,-dependencies- (get-value ,dependencies)))
       (progn 
	 (setq *dependencies* (cons (list ,-target- ,-dependencies-
	 				  (if (listp ',recipe) 
	 				      (function (lambda (target dependencies) ,@recipe)))) 
	 			    *dependencies*))
	 (mapc #'add-file (makelist ,-target-))
	 (mapc #'add-file (makelist ,-dependencies-))
	 ))))

(defun makelist (x)
  "Takes nil, atom, or list and returns a list.  Good for map."
  (if x
      (if (atom x) 
	  (list x) 
	  x)))

(defun run (pgm &rest args)
  (if *verbose*
      (format t "~S~%" (cons pgm args)))
  (run-program pgm args :search :wait :output *verbose*))


(depends "file.o" "file.c" 
	 (run "gcc" "-c" dependencies)
	 )

;(format t "~a~%" *dependencies*)

;(format t "~a~%" (caddar *dependencies*))

;(run-program "ls" nil :search :wait :output *verbose*)

(print (funcall (caddar *dependencies*) 
		(caar *dependencies*) 
		(cadar *dependencies*)))


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
