
(in-package :toolbox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dynamic Programming ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defun-memoized (name-spec arg-list &body body)
  "Define a memoized function, otherwise known as dynamic programming.  This
trys to be about as general as you'll ever need, which makes it somewhat
complicated to understand.  If you don't use any of the fancy options, it should
still work fine for most cases.

To `efficiently' compute Fibonacci numbers:

 (defun-memoized fib (n)
   (if (< n 2) n
       (+ (fib (- n 1))
          (fib (- n 2)) )))

To save computed values in-between calls:

 (defun-memoized (fib :persistent t) (n)
   (if (< n 2) n
       (+ (fib (- n 1))
          (fib (- n 2)) )))

To return the current state of the cache after the run:

 (defun-memoized (fib :return-cache t) (n)
   (if (< n 2) n
       (+ (fib (- n 1))
          (fib (- n 2)) )))

The behavior of the memoization is controlled by the NAME-SPEC.  It has the following form:

name-spec = name
          | (name &key persistent return-cache
                       (arg-fn ''identity)
                       access-fn setter-fn
                       (cached-test #'second)
                       (storage '(make-hash-table :test 'equal)) )

PERSISTENT decides whether the memoization will remain in between calls to the
defined function \(non-NIL) or if a fresh cache is created on each call \(NIL).
Memoization libraries usually do this by default.  Dynamic programming tends to
include building the cache as part of the algorithm.  We default to persistent
cache unless RETURN-CACHE is true.

RETURN-CACHE decides whether the function should return the cache along with
it's result.  For some problems the cache is more important the result.  It is
tacked on as the last value.

ARG-FN specifies a function which will be called on the list of function
arguments prior to trying against the cache.  This allows you to remove
arguments that don't actually effect the result, or to re-arrange arguments
prior to trying the cache.

ACCESS-FN, SETTER-FN, CACHE-TEST tells DEFUN-MEMOIZED how to access, set, and
query the cache.  ACCESS-FN is applied on the cache and a list of the \(ARG-FN
processed) arguments.  SETTER-FN is applied on the new value, the cache, and a
list of the \(ARG-FN processed) arguments.  CACHE-TEST is called on the multiple
value list of the ACCESS-FN's value.  These allow you to use other cache
structures than hash-tables.  Again, most memoization libraries use only
hash-tables which are a good fit for most problems.  Dynamic programming tends
to use arrays.  Arrays might have certain performance \(space or time) gains.

STORAGE instructs the macro on how to build a new cache.  If there is a simple
form that will build it, just place it here.  If it is more complicated, specify
a function that, when given the arguments of the initial call to the defined
function, will build a new cache.  Of course this only works when PERSISTENT is
NIL.

For in-depth examples, see the documentation."
  (destructuring-bind (name
                       &key return-cache (persistent (not return-cache))
                       (arg-fn ''identity)
                       access-fn setter-fn
                       (cache-test ''second)
                       (storage '(make-hash-table :test 'equal)) )
      (ensure-list name-spec)
    (with-gensyms (cache args processed-args query sol)
      `(let ((,cache ,(if persistent storage nil)))
         (declare (ignorable ,cache))
         (defun ,name (,@arg-list)
           (let ((,cache ,(if persistent cache
                              `(if (functionp ,storage)
                                   (funcall ,storage ,@arg-list)
                                   ,storage ))))
             (labels
                 ((,name (&rest ,args)
                    (let ((,processed-args (funcall ,arg-fn ,args)))
                      (let ((,query (multiple-value-list
                                        ,(if access-fn
                                             `(apply ,access-fn ,cache ,processed-args)
                                             `(gethash ,processed-args ,cache) ))))
                        (if (funcall ,cache-test ,query)
                            (values-list ,(if access-fn
                                              `(apply ,access-fn ,cache ,processed-args)
                                              `(gethash ,processed-args ,cache) ))
                            (let ((,sol (multiple-value-list
                                            (destructuring-bind
                                                  ,arg-list ,args
                                              ,@body ))))
                              (values-list
                               ,(if setter-fn
                                    `(apply ,setter-fn ,sol ,cache ,processed-args)
                                    `(setf (gethash (funcall ,arg-fn ,args) ,cache)
                                           ,sol )))))))))
               (declare (notinline ,name))
               ,(if return-cache
                    `(values-list
                      (append (multiple-value-list (,name ,@arg-list))
                              (list ,cache) ))
                    `(,name ,@arg-list) ))))))))

(defmacro defun-array-memoized (name builder return-array arg-list &body body)
  "Define a memoized function using an array for a cache.  BUILDER must specify
a function that will build an array large enough to hold any results.
RETURN-ARRAY is a boolean which determines if the function should return the
cache array as its last value.

This is included as an alternative to the hash-table memoization as it might
have some advantages in terms of speed and memory."
  `(defun-memoized ,(cons name `(:access-fn
                                 #'aref :setter-fn #'(setf aref)
                                 :cache-test #'first
                                 :storage ,builder
                                 :return-cache ,return-array ))
       ,arg-list
     ,@body ))

(defun expand-labels-cache (name-specs)
  (mapcar (/. (name-spec)
             (list cache-sym
                   (destructuring-bind
                         (name
                          &key return-cache
                          (arg-fn ''identity)
                          access-fn setter-fn
                          (cache-test #'second)
                          (storage '(make-hash-table :test 'equal)) )
                       (ensure-list name-spec)
                     storage )))
          name-specs ))

(defun expand-labels-def (defs cache-forms)
  (mapcar
   (lambda (def cache-form)
     (destructuring-bind
           ((name
             &key return-cache
             (arg-fn ''identity)
             access-fn setter-fn
             (cache-test #'second)
             &allow-other-keys )
            arg-list
            &rest body ) def
       (let ((cache (first cache-form)))
         (with-gensyms (args processed-args query sol)
           `(,name (,@arg-list)
                   (labels
                       ((,name (&rest ,args)
                          (let ((,processed-args (funcall ,arg-fn ,args)))
                            (let ((,query (multiple-value-list
                                              ,(if access-fn
                                                   `(apply ,access-fn ,cache
                                                           ,processed-args )
                                                   `(gethash ,processed-args
                                                             ,cache )))))
                              (if (funcall ,cache-test ,query)
                                  (values-list ,(if access-fn
                                                    `(apply ,access-fn ,cache
                                                            ,processed-args )
                                                    `(gethash ,processed-args
                                                              ,cache )))
                                  (let ((,sol (multiple-value-list
                                                  (destructuring-bind
                                                        ,arg-list ,args
                                                    ,@body ))))
                                    (values-list
                                     ,(if setter-fn
                                          `(apply ,setter-fn ,sol ,cache
                                                  ,processed-args )
                                          `(setf (gethash (funcall ,arg-fn ,args)
                                                          ,cache )
                                                 ,sol )))))))))
                     (declare (notinline ,name))
                     ,(if return-cache
                          `(values-list
                            (append (multiple-value-list (,name ,@arg-list))
                                    (list ,cache) ))
                          `(,name ,@arg-list) )))))))
   defs cache-forms ))

(defmacro memo-labels (((name-spec arg-list &body lab-body) &rest more-funcs)
                       &body body )
  "A labels memoized definer for lexically scoped functions.  This is takes the same functions as DEFUN-MEMOIZED, except for the PERSISTENT keyword \(all memoized functions cache for the extent of their scope)."
  (let ((cache-forms (expand-labels-cache (cons name-spec (mapcar #'first more-funcs)))))
    `(let ,cache-forms
       (labels ,(expand-labels-def
                 (mapcar (lambda (x) (cons (ensure-list (first x))
                                      (rest x) ))
                   (cons `(,name-spec ,arg-list ,@lab-body)
                         more-funcs ))
                 cache-forms )
         ,@body ))))



