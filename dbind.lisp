;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; My ``On Lisp'' based tool box ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :toolbox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Destructuring bind ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-compilation-unit (:override nil)

  (defmacro dbind (pat seq &body body)
    (let ((gseq (gensym "DBIND-")))
      `(let ((,gseq ,seq))
         ,(dbind-ex (destruc pat gseq #'atom) body) )))

  (defun destruc (pat seq &optional (atom? #'atom) (n 0))
    (if (null pat)
        nil
        (let ((rest (cond ((funcall atom? pat) pat)
                          ((eq (car pat) '&rest) (cadr pat))
                          ((eq (car pat) '&body) (cadr pat))
                          (t nil) )))
          (if rest
              `((,rest (subseq ,seq ,n)))
              (let ((p (car pat))
                    (rec (destruc (cdr pat) seq atom? (1+ n))) )
                (if (funcall atom? p)
                    (cons `(,p (elt ,seq ,n))
                          rec )
                    (let ((var (gensym "DESTRUC-")))
                      (cons (cons `(,var (elt ,seq ,n))
                                  (destruc p var atom?) )
                            rec ))))))))

  (defun dbind-ex (binds body)
    (if (null binds)
        `(progn ,@body)
        `(let ,(mapcar #'(lambda (b)
                           (if (consp (car b))
                               (car b)
                               b ))
                       binds )
           ,(dbind-ex (mapcan #'(lambda (b)
                                  (if (consp (car b))
                                      (cdr b) ))
                              binds )
                      body )))) )

#| Examples
;;; Destructures and binds on any #1=(sequence of atoms or sequences) of #1#

(dbind (a b c) #(1 2 3)
  (list a b c) )
(dbind (a (b c) d) '(1 #(2 3) 4)
  (list a b c d) )
(dbind (a (b . c) &rest d) '(1 "fribble" 2 3 4)
  (list a b c d) )
(dbind (a (b (c (d (e))))) '(1 #(2 (3 #(4 "5"))))
  (list a b c d e) )

|#

(defmacro with-matrix (pats ar &body body)
  (let ((gar (gensym "WITH-MATRIX-")))
    `(let ((,gar ,ar))
       (let ,(let ((row -1))
               (mapcan
                 #'(lambda (pat)
                     (incf row)
                     (setq col -1)
                     (mapcar #'(lambda (p)
                                 `(,p (aref ,gar
                                            ,row
                                            ,(incf col) )))
                             pat ))
                 pats ))
         ,@body ))))

(defmacro with-array (pat ar &body body)
  (let ((gar (gensym "WITH-ARRAY-")))
    `(let ((,gar ,ar))
       (let ,(mapcar #'(lambda (p)
                         `(,(car p) (aref ,gar ,@(cdr p))) )
                     pat )
         ,@body ))))

(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym "WITH-STRUCT-")))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
                         `(,f (,(symb name f) ,gs)) )
                     fields )
         ,@body ))))

#| Examples
;;; Destructures arrays, matricies, and structures.

(let ((mat (make-array '(3 3) :initial-contents '((00 01 02)
                                                  (10 11 12)
                                                  (20 21 22) ))))
  (with-matrix ((a b c)
                (d e f)
                (g h i) )
               mat
    (print (list a b c d e f g h i))
    (setf a 33) )
  (print mat)
  (with-array ((a 0 0) (b 1 1) (c 2 2)) mat
    (list a b c) ))

(defstruct visitor name title firm)
(defvar theo (make-visitor :name "Theodebert"
                           :title 'king
                           :firm 'franks ))
(with-struct (visitor- name firm title) theo
  (list name firm title) )

|#

;; with-places only works with sequences
;; Also it works by modifying your code to reference the location of the actual
;; memory instead of the local binding (like with-slots)
(with-compilation-unit (:override nil)
  (defmacro with-places (pat seq &body body)
    (let ((gseq (gensym "WITH-PLACES-")))
      `(let ((,gseq ,seq))
         ,(wplac-ex (destruc pat gseq #'atom) body) )))

  (defun wplac-ex (binds body)
    (if (null binds)
        `(progn ,@body)
        `(symbol-macrolet ,(mapcar #'(lambda (b)
                                       (if (consp (car b))
                                           (car b)
                                           b ))
                                   binds )
                          ,(wplac-ex (mapcan #'(lambda (b)
                                                 (if (consp (car b))
                                                     (cdr b) ))
                                             binds )
                                     body )))) )

#| Examples

(let ((lst '(1 (2 3) 4)))
  (with-places (a (b . c) d) lst
    (print (list a b c d))
    (setf a 'uno)
    (setf c '(tre)) )
  lst )

|#

#| Example of symbol-macrolet

(let ((lst '(1 2 3 4 5)))
  (symbol-macrolet ((first (car lst))
                    (setq setf) )
    (setq first 15)
    lst ))

|#

(with-compilation-unit (:override nil)
  (defun match (x y &optional binds)
    (acond2
      ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
      ((binding x binds) (match it y binds))
      ((binding y binds) (match x it binds))
      ((varsym? x) (values (cons (cons x y) binds) t))
      ((varsym? y) (values (cons (cons y x) binds) t))
      ((and (consp x) (consp y) (match (car x) (car y) binds))
       (match (cdr x) (cdr y) it) )
      (t (values nil nil)) ))

  (defun varsym? (x)
    (and (symbolp x) (eq (char (symbol-name x) 0) #\?)) )

  (defun binding (x binds)
    (labels ((recbind (x binds)
               (aif (assoc x binds)
                    (or (recbind (cdr it) binds)
                        it ))))
      (let ((b (recbind x binds)))
        (values (cdr b) b) ))) )

#| Examples

(match '(p a b c a) '(p ?x ?y c ?x))
(match '(a b c) '(a a a))
(match '(a b c) '(a b c)) ; Matched but no bindings
(match '(p ?x) '(p ?x)) ; Also matched but no bindings can be determined

(match '(?x _ b) '(a z _)) ; _ is a wildcard

|#

;;; Compiled implementation

(with-compilation-unit (:override nil)
  (defmacro if-match (pat seq then &optional else)
    `(let ,(mapcar #'(lambda (v) `(,v ',(gensym "IF-MATCH-")))
                   (vars-in pat #'simple?) )
       (pat-match ,pat ,seq ,then ,else) ))

  (defmacro pat-match (pat seq then else)
    (if (simple? pat)
        (match1 `((,pat ,seq)) then else)
        (with-gensyms (gseq gelse "PAT-MATCH-")
          `(labels ((,gelse () ,else))
             ,(gen-match (cons (list gseq seq)
                               (destruc pat gseq #'simple?) )
                         then
                         `(,gelse) )))))

  (defun vars-in (expr &optional (atom? #'atom))
    (if (funcall atom? expr)
        (if (var? expr) (list expr))
        (union (vars-in (car expr) atom?)
               (vars-in (cdr expr) atom?) )))

  (defun var? (x)
    (and (symbolp x) (eq (char (symbol-name x) 0) #\?)) )

  (defun simple? (x) (or (atom x) (eq (car x) 'quote)))

  (defun gen-match (refs then else)
    (if (null refs)
        then
        (let ((then (gen-match (cdr refs) then else)))
          (if (simple? (caar refs))
              (match1 refs then else)
              (gen-match (car refs) then else) ))))

  (defun match1 (refs then else)
    (dbind ((pat expr) . rest) refs
      (cond ((gensym? pat)
             `(let ((,pat ,expr))
                (if (and (typep ,pat 'sequence)
                         ,(length-test pat rest) )
                    ,then
                    ,else )))
            ((eq pat '_) then)
            ((var? pat)
             (let ((ge (gensym "MATCH1-")))
               `(let ((,ge ,expr))
                  (if (or (gensym? ,pat) (equal ,pat ,ge))
                      (let ((,pat ,ge)) ,then)
                      ,else ))))
            (t `(if (equal ',pat ,expr) ,then ,else)) )))

  (defun gensym? (s)
    (and (symbolp s) (not (symbol-package s))) )

  (defun length-test (pat rest)
    (let ((fin (caadar (last rest))))
      (if (or (consp fin) (eq fin 'elt))
          `(= (length ,pat) ,(length rest))
          `(> (length ,pat) ,(- (length rest) 2)) ))) )

#| Interpreted implementation

(defmacro if-match (pat seq then &optional else)
  `(aif2 (match ',pat ,seq)
         (let ,(mapcar #'(lambda (v)
                           `(,v (binding ',v it)) )
                       (vars-in then #'atom) )
           ,then )
         ,else ))

|#

#| Examples

(defun abab (seq)
  (if-match (?x ?y ?x ?y) seq
            (values ?x ?y)
            nil ))

(abab '(hi ho hi ho))

|#

