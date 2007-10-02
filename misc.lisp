
(in-package :toolbox)

;;;; by-elts : cons-free, implicit iteration over vectors

(with-compilation-unit (:override nil)
  (defmacro by-elts ((vecs n-elts &key (elt-type t)) &body body)
    (let ((i (gensym "BY-ELTS-")))
      (condlet (((integerp n-elts) (var-type `(and fixnum (integer 0 ,n-elts)))
                                   (vars     `(,i)) )
                (t                 (var-type 'fixnum)
                                   (vars     `(,i ,n-elts)) ))
               `(dotimes (,i ,n-elts)
                  (declare ,`(type ,var-type ,@vars))
                  ,@(insert-arefs body vecs i elt-type) ))))
 
  (defun quotep (form)
    (and (listp form) (eql 'quote (car form))) )
 
  (defun needs-eval? (form)
    (not (or (atom form) (quotep form))) )
 
  (defun insert-arefs (tree vectors i elt-type)
    (if (null tree)
        nil
        (condlet (((member (car tree) vectors :test #'equal)
                   ; This is one of our vectors.
                   (curr (list `(the ,elt-type (aref ,(car tree) ,i)))) )
                  ((needs-eval? (car tree))
                   ; This is a complex form, we need to recurse down
                   (curr (list (insert-arefs (car tree) vectors i elt-type))) )
                  (t ; Otherwise, this is a simple case, just leave it unchanged
                    (curr (list (car tree)))) )
                 (append curr (insert-arefs (cdr tree) vectors i elt-type)) ))) )

#| Examples

(let ((x #(1 2 3 4))
      (y #(5 4 3 2)) )
  (by-elts ((x y) 4 :elt-type integer)
    (decf x y) )
  x )

(macroexpand-1
  '(by-elts ((x y) 4 :elt-type integer)
     (decf x y) ))

|#

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
    `(if (streamp ,file)
         (do ((,line (funcall ,fn ,file nil ',eof)
                     (funcall ,fn ,file nil ',eof) ))
             ((eql ,line ',eof) nil)
           ,@body )
         (with-open-file (,str ,file :direction :input)
           (do ((,line (funcall ,fn ,str nil ',eof)
                       (funcall ,fn ,str nil ',eof) ))
               ((eql ,line ',eof) nil)
             ,@body )))))

#| Examples
(macroexpand-1
  '(do-file-by-lines #p"fname.file" line
     (print line) ))

(macroexpand-1
'(do-file-by #'read-line (#p"scratch.lisp" line)
  (print line) )
)

(macroexpand-1
'(do-file-by #'read-line (hello line)
  (print line) )
)

(do-file-by #'read-line (#p"scratch.lisp" line)
  (print line) )

(do-file-by #'read (#p"scratch.lisp" line)
  (print line) )

|#

;;;;;;;;;;;;;;;;;;;;;;;
;;;; Head and tail ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; head (reacursive with a depth of n)
(defun head (list &optional (n 10))
  (cond ((or (= n 0) (null list)) nil)
        (t (cons (car list) (head (cdr list) (1- n))) )))

;;; tail (recursive with a depth equal the the list length)
(defun tail (list &optional (n 10))
  (cond ((null list) (values nil n))
        (t (mvb (ret count) (tail (cdr list) n)
             (if (< 0 count)
                 (values list (1- count))
                 (values ret count) )))))

#| Examples
(head '(1 2 3 4 5 6 7 8 9 10 11 14 51))

(tail '(1 2 3 4 5 6 7 8) 4)

|#

(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
      (make-array dims :displaced-to array)
      dims )))

