;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; My ``On Lisp'' based tool box ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utility functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :toolbox)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Anaphoric macros ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
     ((not it))
     ,@body ))

#|
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form) ))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body) ))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args)))) ))


(defmacro acond (&rest clauses)
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (sym (gensym "ACOND-")) )
      `(let ((,sym ,(car cl1)) )
         (if ,sym
           (let ((it ,sym)) ,@(cdr cl1))
           (acond ,@(cdr clauses)) )))))
|#

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self ))

(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
                 (case (length args)
                   (0 nil)
                   (1 (car args))
                   (t `(let ((it ,(car args)))
                         ,(self (cdr args)) ))))
               args )))

#| Examples

(macroexpand-1
  '(aif (member 1 '(2 3 1 4 5))
        (reverse it) ))

(aand (member 3 '(1 2 3 4 5)) (reverse it))

(let ((var 99))
  (acond ((member var '(4 3 1 2 3)) (reverse it))
         ((find-if #'evenp '(3 5 7 2)) (print (- it 5)))
         (t 'otherwise) ))

;;; Extremely usefull, it allows you to create `nameless' recursive functions
(macroexpand-1 '(alambda (lst)
                  (cond ((null lst) nil)
                        (t (self (cdr lst))) )))
(funcall (alambda (lst)
           (cond ((null lst) nil)
                 (t (cons lst (self (cdr lst)))) ))
         '(1 2 3 4 5) )

|#

;;; Anaphoric macros that check secondary return values for success

(defmacro aif2 (test &optional then else)
  (let ((win (gensym "AIF2-")))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else) )))

(defmacro awhen2 (test &body body)
  `(aif2 ,test
         (progn ,@body) ))

(defmacro awhile2 (test &body body)
  (let ((flag (gensym "AWHILE2-")))
    `(let ((,flag t))
       (while ,flag
         (aif ,test
              (progn ,@body)
              (setq ,flag nil) )))))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym "ACOND2-"))
            (win (gensym "ACOND2-")) )
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val)) ,@(cdr cl1))
               (acond2 ,@(cdr clauses)) )))))

#| Examples

;;; The meat of a memoized function
(macroexpand
  '(aif2 (gethash args hash)
         it
         (setf (gethash args hash)
               (funcall fn args) )))

(macroexpand
  '(awhen2 (gethash key hash)
     it ))

(macroexpand
  '(awhile2 (gethash key hash)
     (do-some-stuff)
     (maybe-set (gethash key hash)) ))

(macroexpand
  '(acond2 ((test1) it)
           ((test2) (not it))
           (t       'goodbye) ))

|#

(with-compilation-unit (:override nil)

  (defmacro t-ret (&body body)
    "A macro that will change the success value of a predicate to an
    arbitrary value of your choosing.  This is helpful for pedicates 
    that return t and not something more useful, making them annoying 
    to use, expecially with anaphoric macros."
    (let ((ret-sym (gensym "T-RET-")))
      (multiple-value-bind (new-body ret-val)
                            (extract-ret-call body ret-sym)
        `(let ((,ret-sym ,ret-val))
           (if ,@new-body ,ret-sym) ))))

  (defun extract-ret-call (tree ret-val)
    (cond ((atom tree) (values tree nil))
          ((eql (car tree) :ret) (values ret-val (cadr tree)))
          (t (multiple-value-bind (body1 val1) (extract-ret-call (car tree) ret-val)
               (multiple-value-bind (body2 val2) (extract-ret-call (cdr tree) ret-val)
                 (values (cons body1 body2) (or val1 val2)) ))))) )

#| Examples

(extract-ret-call '(> (+ 3 (:ret (+ 2 5))) 4) (gensym))

(macroexpand-1 '(t-ret (> (+ 3 (:ret (+ 2 3))) 2)))

(aif (t-ret (> (+ 3 (:ret (+ 2 3))) 2))
     (- it 5)
     nil )

(t-ret (:ret (read)))

(let ((seq1 '(1 2 3 4))
      (seq2 '(1 2 3))
      (seq3 '(5 4 3)) )
  (aif (t-ret (> (:ret (length seq1)) (length seq2)))
       (if (> it (length seq3))
           (aif (t-ret (not (= 0 (:ret (car (last seq1))))))
                it ))))

|#

(with-compilation-unit (:override nil)

  (defmacro a+ (&rest args)
    "`it' bound to the previous term in the addition"
    (a+expand args nil) )

  (defun a+expand (args syms)
    (if args
        (let ((sym (gensym "A+EXPAND-")))
          `(let* ((,sym ,(car args))
                  (it ,sym) )
             ,(a+expand (cdr args)
                        (append syms (list sym)) )))
        `(+ ,@syms) )) )

(with-compilation-unit (:override nil)

  (defmacro alist (&rest args)
    "`it' bound to the previous term in the list"
    (alist-expand args nil) )

  (defun alist-expand (args syms)
    (if args
        (let ((sym (gensym "ALIST-EXPAND-")))
          `(let* ((,sym ,(car args))
                  (it ,sym) )
             ,(alist-expand (cdr args)
                            (append syms (list sym)) )))
        `(list ,@syms) )) )

(with-compilation-unit (:override nil)

  (defmacro defanaph (name &key calls (rule :all))
    "A macro for automating anahporic macro definitions."
    (let* ((opname (or calls (pop-symbol name)))
           (body (case rule
                   (:all `(anaphex1 args '(,opname)))
                   (:first `(anaphex2 ',opname args))
                   (:place `(anaphex3 ',opname args)) )))
      `(defmacro ,name (&rest args)
         ,body )))

  (defun anaphex1 (args expr)
    (if args
        (let ((sym (gensym "ANAPHEX1-")))
          `(let* ((,sym ,(car args))
                  (it ,sym) )
             ,(anaphex (cdr args)
                       (append expr (list sym)) )))
        expr ))

  (defun anaphex2 (op args)
    `(let ((it ,(car args))) (,op it ,@(cdr args))) )

  (defun anaphex3 (op args)
    `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)) )

  (defun pop-symbol (sym)
    (intern (subseq (symbol-name sym) 1)) ) )

#| Examples

;;; These are not the most useful, perhaps they are better as examples
(a+ 1 2 (/ 1 it) 4 (* 0.1 it))
(alist 1 (+ it 1) (+ it 1))

(pop-symbol 'aif)
(pop-symbol 'acond)

|#

