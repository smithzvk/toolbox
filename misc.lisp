
(in-package :toolbox)

;;;; BY-ELTS : cons-free, implicit iteration over vectors
;;;; is this needed anymore?
(with-compilation-unit (:override nil)
  (defmacro by-elts (vec-spec &body body)
    (mvb (vecs specs) (parse-vec-spec vec-spec)
      (let* ((itr-syms (get-gensyms (length (car specs)) "BY-ELTS-"))
             (min-syms (get-gensyms (length (car specs)) "BY-ELTS-"))
             (max-syms (get-gensyms (length (car specs)) "BY-ELTS-"))
             (vec-syms (get-gensyms (length vecs) "BY-ELTS-")) )
        (expand-env-and-loops vecs specs itr-syms min-syms min-syms max-syms max-syms
                              vec-syms body ))))
  (defun expand-env-and-loops (vecs specs i-sym min-sym min-orig max-sym
                               max-orig v-sym body )
    (if (null min-sym)
        `(let ,(mapcar (cut (list <> <>)) v-sym vecs)
           (symbol-macrolet ,(mapcar (cut (list <> `(aref ,<> ,@i-sym))) vecs v-sym)
             ,(expand-loops i-sym min-orig max-orig body) ))
        `(let ((,(car min-sym) ,(cadaar specs))
               (,(car max-sym) ,(car (cddaar specs))) )
           ,(expand-env-and-loops vecs (cdr specs) i-sym (cdr min-sym) min-orig
                                  (cdr max-sym) max-orig v-sym body ))))
  (defun expand-loops (i-sym min max body)
    (if (null i-sym)
        `(progn ,@body)
        `(do ((,(car i-sym) ,(car min) (1+ ,(car i-sym))))
           ((= ,(car i-sym) ,(car max)))
           ,(expand-loops (cdr i-sym) (cdr min) (cdr max) body) )))
  (defun parse-vec-spec (vec-spec)
    (if (null vec-spec) (values () ())
        (mvb (vecs specs) (parse-vec-spec (cdr vec-spec))
          (let ((spec (get-spec (car vec-spec))))
            (if (atom (car vec-spec))
                (values (cons (car vec-spec) vecs)
                        (cons spec specs) )
                (values (cons (caar vec-spec) vecs)
                        (cons spec specs) ))))))
  (defun parse-spec (vec spec dim)
    (cond ((null spec) ())
          ((eql :all (car spec))
           (cons `(:range 0 (array-dimension ,vec ,dim))
                 (parse-spec vec (cdr spec) (1+ dim))) )
          ((eql :list (caar spec))
           (error "Index lists are not yet implemented") )
          ((eql :range (caar spec))
           (cons (car spec) (parse-spec vec (cdr spec) (1+ dim))) )
          (t (error "Unknown specifier")) ))
  (defun get-spec (vec-spec)
    (if (atom vec-spec) ; Set default spec (every element of a vector)
        `((:range 0 (length ,vec-spec)))
        (parse-spec (car vec-spec) (cdr vec-spec) 0) )) )

;; ;; Examples

;; (let ((x (copy-array #(1 2 3 4)))
;;       (y (copy-array #(5 4 3 2))) )
;;   (by-elts (x y)
;;     (decf x y) )
;;   x )

;; (array-dimensions #2A((1 2) (3 4)))

;; (let ((x (copy-array #2A((1 2 3) (4 5 6) (7 8 9))))
;;       (y (copy-array #2A((11 22 33) (44 55 66) (77 88 99))))
;;       (z (make-array '(3 3) :initial-element 0)) )
;;   (by-elts ((x :all :all) y z)
;;     (setf z (+ x y)) )
;;   z )

;; (let ((x (copy-array #(1 2 3 4)))
;;       (y (copy-array #(5 4 3 2))) )
;;   (by-elts ((x (:range 1 3)) (y (:range 0 2)))
;;     (decf x y) )
;;   x )

;; (macroexpand-1
;;   '(by-elts (x y)
;;      (decf x y) ))

;; (macroexpand-1
;;   '(by-elts ((x (:range 0 2)) (y (:range 1 3)))
;;      (decf x y) ))

;; (macroexpand-1
;;   '(by-elts ((x :all :all) (y :all :all))
;;      (decf x y) ))

;; ;;; Look what symbol-macrolet will do!!
;; (let ((vec #(1 2 3)))
;;   (let ((vec2 vec)) ; To avoid recursive expansion
;;     (symbol-macrolet ((vec (aref vec2 0)))
;;       (print vec)
;;       (let ((vec #(2 3 4))) ; the symbol-macrolet binding is shadowed.
;;         (print vec) )
;;       vec )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Text file operations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro do-file-by-lines ((filename line) &body body)
  (let ((str (gensym "DO-FILE-BY-LINES-")))
    `(with-open-file (,str ,filename :direction :input)
       (do ((,line (read-line ,str nil) (read-line ,str nil))) ((not ,line) nil)
         ,@body ))))

(defmacro do-file-by (fn (file line) &body body)
  (with-gensyms (str eof "DO-FILE-BY-")
    `(with-open-file (,str ,file :direction :input)
       (do ((,line (funcall ,fn ,str nil ',eof)
                   (funcall ,fn ,str nil ',eof) ))
         ((eql ,line ',eof) nil)
         ,@body ))))

;;  Examples
;; (macroexpand-1
;;   '(do-file-by-lines #p"fname.file" line
;;      (print line) ))

;; (macroexpand-1
;; '(do-file-by #'read-line (#p"scratch.lisp" line)
;;   (print line) )
;; )

;; (macroexpand-1
;; '(do-file-by #'read-line (hello line)
;;   (print line) )
;; )

;; (do-file-by #'read-line (#p"scratch.lisp" line)
;;   (print line) )

;; (do-file-by #'read (#p"scratch.lisp" line)
;;   (print line) )

;;;;;;;;;;;;;;;;;;;;;;;
;;;; Head and tail ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; head
(defun head (list &optional (n 10))
  (subseq list 0 n) )

(defun unroll-circular-list (circular-list n)
  "Create a proper list out of CIRCULAR-LIST that has the same
elements repeating to make a list of length N."
  (cond ((= n 1) (list (car circular-list)))
        (t (cons (car circular-list)
                 (unroll-circular-list (cdr circular-list) (1- n)) ))))

;;; tail (iterative)
(defvar *empty-sym*
  (gensym "EMPTY-")
  "This symbol is used to mark unused elements.  I need a name that
  the user cannot use, hence the gensym." )
(defun tail (lst n)
  "Return the last N elements of LiST.  This does not create a new
list.  As far as lists go, this is about as good as it gets."
  (do ((lst lst (cdr lst))
       (tail (n-times n (curry #'cons *empty-sym*) lst)
             (cdr tail) ))
      ((and (null lst) (not (eql *empty-sym* (car tail))))
       tail )))

;; ;; Examples

;; (head '(1 2 3 4 5 6 7 8 9 10 11 14 51))

;; (tail '(1 2 3 4 5 6 7 8) 4)

;; (defun copy-array (array)
;;   (let ((dims (array-dimensions array)))
;;     (adjust-array
;;       (make-array dims :displaced-to array)
;;       dims )))

(defun fsubvec (vec &optional (start 0) (end (length vec)))
  "Access a subsequence from a vector.  Do this with displaced arrays,
thus we are not consing or copying, but are pointing to the same
memory.  When we are dealing with functional code, this doesn't matter
and removing the copying is faster."
  (make-array (- end start)
              :displaced-to vec
              :displaced-index-offset start
              :element-type (array-element-type vec)
              :fill-pointer (and (array-has-fill-pointer-p vec)
                                 (fill-pointer vec) )
              :adjustable (adjustable-array-p vec) ))

;; (abbrev /. lambda)
;; Do it this way so we know the lambda list.  Can we make abbrev so
;; it doesn't lose this information?  It would have to have the source.
(defmacro /. (args &body body)
  `(lambda ,args ,@body) )

(defmacro defwrapper (wrapper func &optional comment)
  "Create a wrapper function for a function.  This allows for a general
  interface with several possible backends, i.e. PRIME? could wrap MILLER-RABIN,
  or the faster (less accurate) PSEUDO-PRIME, or perhaps a slow but
  deterministic polynomial time algorithm by Agrawai, Kayal, and Saxena.
  
  It also creates an intelligent documentation string."
  (let ((comment (if comment comment
                     ;; Generate a sensible comment
                     (concatenate 'string
                                  (mkstr wrapper)
                                  " is an automatically generated wrapper function for "
                                  (mkstr func)
                                  #(#\: #\Space #\Newline)
                                  (documentation func 'function) ))))
      `(progn
         ;; Inline the function.  We don't want to waste stack frames on sugar
         (declaim (inline ,wrapper))
         (defun ,wrapper (&rest args)
           ,comment
           (apply #',func args) ))))

;; ;; Examples

;; (macroexpand-1
;;   '(defwrapper prime? miller-rabin) )

(defun get-external-symbols (pkg)
  "Returns a list of the external symbols in PacKaGe."
  (let (ret)
    (do-external-symbols (sym pkg ret)
      (push sym ret) )))

(defun use-package-excluding (from-package shadows &optional (to-package *package*))
  "Like USE-PACKAGE except do not import the external symbols listed
in SHADOWS."
  (mapcar (rcurry #'import to-package)
          (remove-if (rcurry #'member shadows)
                     (get-external-symbols from-package) )))

;; ;; Example

;; (asdf:oos 'asdf:load-op 'verrazano)

;; (get-external-symbols :verrazano)

;; (mapcar #'describe (get-interface :verrazano))

;;; I usually replace the dubugger, this puts it back to the course of
;;; the BODY.
;; Not since Slime
;; (defmacro with-debug (&body body)
;;   "Enable the debugger when processing BODY."
;;   `(let ((*debugger-hook* nil))
;;      ,@body ))

(defun n-times (n func arg)
  "Self compose FUNC N times with and call on argument ARG."
  (declare (type (integer 0) n))
  (cond ((= n 0) arg)
        (t (funcall func (n-times (1- n) func arg))) ))

(defmacro mapcro (macro &rest args)
  "MAPcar maCRO: Expands into something like what you would expect if
MACRO was a function and you MAPCARed it on ARGS.  Since this is a
macro, ARGS is not evaluated, which can lead to some mistakes if you
think of it too much like MAPCAR.

For example:
  (macroexpand-1 '(mapcro defparameter (x y) (1 2)))
  ==> (progn (defparameter x 1) (defparameter y 2))
  (macroexpand-1 '(mapcro defparameter '(x y) '(5 6)))
  ==> (progn (defparameter quote quote) (defparameter (x y) (5 6))) ; error"
  `(progn ,@(apply #'mapcar
              (lambda (&rest args2)
                `(,macro ,@args2) )
              args )))

;; ;; Examples
;; (macroexpand-1 
;;   '(mapcro defparameter (xxx yyy zzz) (1 2 3)) )

(defmacro nested-dotimes ((vars extent &optional result) &body body)
  "Expands to nested DOTIMES macros.  Performs BODY with the counters
in VARS bound to each of the (APPLY #'* EXTENT) possibilties,
optionally returning RESULT."
  (labels ((expand-loops (vars extent body)
             (cond ((null vars) body)
                   (t (append
                       `((dotimes (,(car vars) (car ,extent))
                          ,@(expand-loops (cdr vars)
                                         `(cdr ,extent)
                                         body ))))))))
    `(dotimes (,(car vars) (first ,extent) ,result)
       ,@(expand-loops (cdr vars) `(cdr ,extent) body) )))

