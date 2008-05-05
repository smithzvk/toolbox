;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dynamic Programming ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defun-dynammic (name-options args &body body)
  (list name-options args body) )

(macroexpand-1 '(defun-dynammic manhattan (a b c) hello))

(make-symbol "HELLO")

(with-gensyms ("Hello")
  5 )

#|
;;; This would be a cool tool.  We would need to use a code walker to walk the
;;; _returned_ form in order to find free symbols and gensym them.
(defmacro defmacro-hyg (name args &body body)
  (let ((free-symbols (collect-free-symbols args body)))
    `(defmacro ,name ,args
       (with-gensyms ,free-symbols))))
|#

(dbind (name &key val win) '(hello :val 5)
  (list name val win) )

(destructuring-bind (name &key (test #'eql)) '(hello :test #.#'car)
  (list name test) )

(defun default-memoization (name)
  (list name :test #'eql :key #'identity) )

(defmacro defun-dynprog (name-spec arg-list &body body)
  (with-gensyms (cache "DYN-PROG-")
    `(let ((,cache (make-hash-table)))
       (defun ,name-spec ,arg-list
         (let ((,cache (make-hash-table)))
           (funcall
             (alambda ,arg-list
               (declare (notinline self))
               (macrolet ((,name-spec ,arg-list
                            '`,(self ,@arg-list) ))
                 ,@body ))
             ,@arg-list ))))))

(macroexpand-1 '(defun-dynprog fact (n) (cond ((= n 1) 1) (t (* n (fact (1- n)))))))

(defun-dynprog fib (n)
  (cond ((< n 3) n)
        (t (+ (fib (1- n)) (* n (fib (- n 2))))) ))

(trace fib)

(fib 2)

(defun dyn-label (name args &rest fbody)
  (let ((label-spec
          (if (atom name)
              (default-memoization name)
              name ))
        (name-internal 'internal) )
    (destructuring-bind (name &key test key) label-spec
      (values 'cache
              `(,name ,args
                 (setf cache (make-hash-table :test ,test))
                 (,name-internal ,@args) )
              `(,name-internal ,args
                 (declare (notinline ,name-internal))
                 (mvb (val win) (gethash (funcall ,key ,args) cache)
                      (if win
                          val
                          (setf (gethash (funcall ,key ,args) cache)
                                (macrolet
                                  ((,name ,args
                                     (,name-internal ,@args) ))
                                  ,@fbody )))))))))

(defmacro dyn-labels (label-specs &body body)
  (multiple-value-bind (cache-names cache-decl funcs)
                       (mvmapcar (/. (x) (apply #'values (mvl (apply #'dyn-label x)))) label-specs)
    (print (list cache-names cache-decl funcs))
    `(let ,cache-decl
       (labels ,funcs
         ,@body ))))

(apply #'floor '(3 2))

(mvmapcar (lambda (x) (apply #'dyn-label x))
          '((hello (a b c) (hello (1- a) (1- b) (1- c)))) )

(mvmapcar #'floor '((3 2) (4 3) (5 4)))

(asdf:oos 'asdf:load-op :cl-ode)

(with-debug
(macroexpand-1 '(dyn-labels ((hello (a)
                               (cond ((= a 1) 1)
                                     (t (* a (hello (1- a)))) )))
                  (hello 5) ))
)

(describe 'apply)

(defun mvmapcar (fn &rest lists)
  (cond ((null (car lists)) (apply #'values lists))
        (t (apply #'values 
                  (mapcar #'cons (mvl (apply fn (mapcar #'car lists)))
                          (mvl (apply #'mvmapcar fn (mapcar #'cdr lists))) )))))

(mvmapcar #'values '(1 2 3) '(4 5 6))

(caddr '#1=(1 #1# (2 #1#)))

'#1=(1 . #1#)

(setf *print-circle* t)

(asdf:oos 'asdf:load-op :pal)

#|

defun-dyn:

1. Define a function that creates a function with a cache that saves previous evaluations

2. Calling the function creates a (clean) cache and calls the internal function with the proper arguments

|#

(defun mv-mapcar (fn &rest lists)
  (

(mapcar (/. (x y) (mvl (floor x y))) '(1 2 3) '(3 2 1))

(macroexpand-1 '(dyn-labels (dyn (a b c) (list a b c)) (dyn 1 2 3)))


(let ((cache (make-hash-table)))
  (defun dyn (mat x y)
    (labels ((%dyn (&rest args)
               (asif2 (gethash args cache)
                      it
                      (setf it (apply %dyn args)) )))
      (%dyn 
               (mvb (val win) (gethash args cache)
                    (
  (defun %dyn (x y)
    (



