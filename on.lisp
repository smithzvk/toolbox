;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; My ``On Lisp'' based tool box ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :toolbox)

;;;;;;;;;;;;;;;;;;;;;;
;;;; Utility functions

;;; List utilities
(proclaim '(inline last1 single append1 conc1 mklist))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun last1 (lst)
    (car (last lst)) ))

(defun single (lst)
  (and (consp lst) (not (cdr lst))) )

(defun append1 (lst obj)
  (append lst (list obj)) )

(defun conc1 (lst obj)
  (nconc lst (list obj)) )

(defun mklist (obj)
  (if (listp obj) obj (list obj)) )

;; Examples
;; (last '(1 2 3 4))
;; (last1 '(1 2 3 4))

;; (single '(1))
;; (single '(1 2))

;; (append1 '(1 2 3 4) 5)

;; (mklist '(5))
;; (mklist 5)

(defun longer-than (n list)
  (if (or (= n 0) (not list))
      list
      (longer-than (1- n) (cdr list)) ))

(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y)) ))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)) )))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc)) ))
    (nreverse acc) ))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                      (rec rest (cons (subseq source 0 n) acc))
                      (nreverse (cons source acc)) ))))
    (if source (rec source nil) nil) ))

(defun group-by (list &rest counts)
  (let ((ret list))
    (dolist (cnt counts ret)
      (setf ret (group ret cnt)) )))

;; (defun flatten (x)
;;   (labels ((rec (x acc)
;;              (cond ((null x) acc)
;;                    ((atom x) (cons x acc))
;;                    (t (rec (car x) (rec (cdr x) acc))) )))
;;     (rec x nil) ))

(defun flatten-array (array)
  (make-array (apply #'* (array-dimensions array))
              :displaced-to array ))

(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc) ))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                             acc
                             (cons (car tree) acc) ))))))
    (rec tree nil) ))

(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst)) ))))

;; (defun before (x y lst &key (test #'eql))
;;   "Is the first occurence of X before Y in LST using TEST"
;;   (and lst
;;        (let ((first (car lst)))
;;          (cond ((funcall test y first) nil)
;;                ((funcall test x first) lst)
;;                (t (before x y (cdr lst) :test test)) ))))

;; (defun after (x y lst &key (test #'eql))
;;   "Is the first occurence of X after Y in LST using TEST"
;;   (let ((rest (before y x lst :test test)))
;;     (and rest (member x rest :test test)) ))

;; ;; Examples

;; ;;; Similar to find-if, member-if but returns the evaluation as the 
;; ;;; second argument
;; (find2 #'evenp '(1 2 3 4 5))
;; (find2 (cut (member 5 <>)) '((1 2 3) (2 7 6 3) (3 5 3) (4) (5 9 2)))
;; (find-if #'evenp '(1 2 3 4 5))
;; (member-if #'evenp '(1 2 3 4 5))

;; ;;; Is the first occurence of arg1 before/after the first occurence of arg2?
;; (before 'a 'b '(1 2 3 b 5 a))
;; (before 'a 'b '(1 a 2 3 b 5 a))

;; (after 'a 'b '(1 2 3 b 5 a))
;; (after 'a 'b '(1 a 2 3 b 5 a))

(defun duplicate (obj lst &key (test #'eql) (key #'identity))
  (member obj (cdr (member obj lst :test test :key key))
          :test test :key key ))

(defun n-duplicate (n obj lst &key (test #'eql) (key #'identity))
  (if (= n 1)
      (member obj lst :test test :key key)
      (member obj (cdr (n-duplicate (1- n) obj lst :test test :key key))
              :test test :key key )))

(defun split-on (fn lst)
  "Split the LiST at the first element where function, FN, is non-NIL.
    Lists returned as multiple values."
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
           (values (nreverse acc) src) )
      (push (car src) acc) )))

(defun split-if (fn lst)
  "Split the LiST in two.  The first list contains elements of the
list that satisfy function FN and the second contains elements that
don't.  Lists are returned as multiple values."
  (unless (null lst)
    (multiple-value-bind (on off) (split-if fn (cdr lst))
      (if (funcall fn (car lst))
          (values on (cons (car lst) off))
          (values (cons (car lst) on) off) ))))

;; ;; Examples

;; (duplicate 1 '(1 2 1 3))
;; (duplicate 2 '(1 2 1 3))

;; (duplicate 1 '((H . 1) (E . 2) (J . 1) (1 . 3)) :key #'cdr)

;; (n-duplicate 3 1 '((H . 1) (E . 2) (J . 1) (1 . 3) (5 . 1)) :key #'cdr)

;; (split-on #'oddp '(1 2 3 4))

;; (split-if #'oddp '(2 2 3 4))

;; (split-if (cut (> <> 4)) '(1 2 3 4 5 6 7 8))

(defun most (fn lst)
  (if (null lst)
    (values nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)) )
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setq wins obj
                  max  score ))))
      (values wins max) )))

(defun best (fn lst)
  (if (null lst)
    nil
    (let ((wins (car lst)))
      (dolist (obj (cdr lst))
        (if (funcall fn obj wins)
          (setq wins obj) ))
      wins )))

(defun mostn (fn lst)
  (if (null lst)
    (values nil nil)
    (let ((result (list (car lst)))
          (max (funcall fn (car lst))) )
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((> score max)
                 (setq max    score
                       result (list obj) ))
                ((= score max)
                 (push obj result) ))))
      (values (nreverse result) max) )))

;; ;; Examples

;; (most #'length '((a b) (c d e) (f) (g h i)))

;; (best #'> '(1 2 3 4 5 3 2))

;; (mostn #'length '((a b) (a b c) (a) (e f g)))

;;; Function versions of some macros

(defun all (&rest x)
  "Like the macro `and' except a function, so no flow control, etc."
  (every #'identity x) )

(defun any (&rest x)
  "Like the macro `or' except a function, so no flow control, etc."
  (some #'identity x) )

;; ;; Examples

;; (all 1 2 3 4)
;; (all 1 nil t 'false)
;; (any nil nil t nil)
;; (any nil nil)

;;; Some pseudo-predicate > < functions
(defun _> (a b)
  (declare (inline _>))
  (if (> a b) a nil) )

(defun >_ (a b)
  (declare (inline >_))
  (if (> a b) b nil) )

(defun _< (a b)
  (declare (inline _<))
  (if (< a b) a nil) )

(defun <_ (a b)
  (declare (inline <_))
  (if (< a b) b nil) )

;;; Mapping utilities

(defun map-> (fn start test-fn succ-fn)
  "Create a list with 'fn' applied to it's values which start at 'start' with
  each successive value given by 'succ-fn' applyed to the value until 'test-fn'
  applied to the value returns true."
  (do ((i start (funcall succ-fn i))
       (result nil))
    ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result) ))

(defun mapa-b (fn a b &optional (step 1))
  "Create list with values from 'a' to 'b' spaced by 'step' with 'fn' applied to it"
  (do ((i a (+ i step))
       (result nil) )
      ((> i b) (nreverse result))
    (push (funcall fn i) result) ))

(defun map1-n (fn n)
  (mapa-b fn 1 n) )

(defun map0-n (fn n)
  (mapa-b fn 0 n) )

;; (defun mappend (fn &rest lsts)
;;   (apply #'append (apply #'mapcar fn lsts)) )

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result) ))
    (nreverse result) ))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
    (apply fn args)
    (apply #'mapcar
           #'(lambda (&rest args)
               (apply #'rmapcar fn args) )
           args )))

;; ;; Examples

;; (map0-n #'1+ 5)

;; ; Can only map unary functions over the range)
;; (mapa-b #'identity 1 8)

;; (mapa-b #'+ 1 4 0.2d0)

;; (map-> #'/              ; function to map on (once again unary only)
;;        1                ; start value
;;        #.(cut (> <> 3)) ; exit clause
;;        #'1+ )           ; next element

;; ; Not really sure what this is good for
;; (mappend #'identity '((1 3 4) (2 10 11) (3 2 1) (4 5 6)))

;; ; Cool stuff you can do with cut
;; (mapcar (cut (apply #'+ <>)) '((1 2 3) (4 5 6)))
;; (mapcar (cut (reduce #'+ <>)) '((1 2 3) (4 5 6)))

;; ;Series code, doesn't seem to work?
;; ;(collect (#Mfn (scan-range :from a :upto b :by c)))

;; ; Mapping over several lists without consing
;; (mapcars #'sqrt '(1 2 3 4) '(4 5 6 7)) ; == (mapcar #'sqrt (append '(1 2 3 4) '(4 5 6 7)))

;; ; Recursive mapcar
;; (rmapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40)))

;;; break loop stuff

(defun readlist (&rest args)
  (values (read-from-string
            (concatenate 'string "("
                                 (apply #'read-line args)
                                 ")" ))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*) )

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop. ~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
        (return)
        (format *query-io* "~a~%" (funcall fn in)) ))))

(defun break-toplevel ()
  (break-loop #'eval #'(lambda (x) (eq x :q)) ">> ") )

;;; Symbol/string utilities

(defun mkstr (&rest args)
  "MaKe STRing"
  (with-output-to-string (s)
    (dolist (a args) (princ a s)) ))

(defun mkdstr (&rest args)
  "MaKe space Delimited STRing"
  (apply #'mkdstr* " " args) )

(defun mkdstr* (delimiter &rest args)
  "MaKe arbitrarily Delimited STRing"
  (let ((new-args (shuffle args (make-list (1- (length args))
                                           :initial-element delimiter ))))
    (with-output-to-string (s)
      (dolist (a new-args)
        (princ a s) ))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))) )

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))) )

(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1 :initial-element c)) )
       (symbol-name sym) ))

;; ;; Examples

;; (mkstr 'hello)
;; (mkstr 1984)
;; (mkstr '(1 2 3 4))
;; (mkstr (make-hash-table))
;; (string 'howdy)
;; ;(string 1984)       ;error
;; ;(string '(1 2 3 4)) ;error
;; (symbol-name 'heck)

;; ;;; Largely useless...
;; (symb 'hello)
;; (type-of (symb 'hello))

;; (reread '(1 2 3 4))

;; (explode 'hello)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Function returning functions

(defun memoize (fn &key (size nil size-p) (test #'equalp)
                        (arg-fn #'identity) )
  "Function memoizer"
  (let ((cache (if size-p
                   (make-hash-table :size size :test test)
                   (make-hash-table :test test)) ))
;    (defun clear-memos () (setf cache (make-hash-table :test test)))
;    (defun get-hash () cache)
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash (funcall arg-fn args) cache)
        (if win
            (apply #'values val)
            (apply #'values
                   (setf (gethash (funcall arg-fn args) cache)
                         (multiple-value-list (apply fn args)) )))))))

;; ;; Examples

;; ;;; See string-algs.lisp for dynamic programming examples

;; (declaim (optimize (speed 3) (debug 0)))

;; (defun slow-func (x)
;;   (declare (notinline slow-func))
;;   (cond ((= 0 x) 0)
;;         (t (or (sleep x) (slow-func (1- x)))) ))

;; (setf (symbol-function 'slow-func) (memoize #'slow-func))

;; (slow-func 1)
;; (slow-func 2)

;; (slow-func 1) ; The sleep is not executed, it just remembers the answer and returns it
;; (slow-func 2)

;; (defun compose (&rest fns)
;;   "Function compose"
;;   (if fns
;;     (let ((fn1 (last1 fns))
;;           (fns (butlast fns)) )
;;       #'(lambda (&rest args)
;;           (reduce #'funcall fns
;;                   :from-end t
;;                   :initial-value (apply fn1 args) )))
;;     #'identity ))

(defun fif (if then &optional else)
  "Functional if"
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)) )))

(defun fint (fn &rest fns)
  "Function INTersection"
  (if (null fns)
    fn
    (let ((chain (apply #'fint fns)))
      #'(lambda (x)
          (and (funcall fn x) (funcall chain x)) ))))

(defun fun (fn &rest fns)
  "Function UNion"
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x)) ))))

;; ;; Examples

;; (mapcar (compose (cut (list 'begin <> 'end)) #'1+) '(1 2 3 4 5))

;; (mapcar (compose #'sqrt ; takes 1 argument
;;                  #'/ )  ; takes 2 arguments
;;         '(1 2 3 4 5) '(5 4 3 2 1) )

;; ; Move up to the next even integer
;; (funcall (fif #'oddp #'1+ #'identity) 4)

;; ; Integers which are odd and even (i.e. non of them)
;; (mapcar (fint #'oddp #'evenp) '(1 2 3 4))

;; ; Integers which are odd or even (i.e. all of them)
;; (mapcar (fun #'oddp #'evenp) '(1 2 3 4))

(defun lrec (rec &optional (base #'identity))
  "List RECursion function generator:
  Returns a function that accepts a list and if NIL it returns/calls base
  and if a list, calls REC with the first element of the list and a function
  that recurses the funtion on the CDR."
  (declare (type (function (t function) t) rec))
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base )
                 (funcall rec (car lst)
                          #'(lambda ()
                              (self (cdr lst)) )))))
    #'self ))

;; ;; Examples

;; ;length
;; (lrec #'(lambda (x f) (1+ (funcall f))) 0)

;; ;every (for oddp)
;; (lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)

;; ;copy-list
;; (lrec #'(lambda (x f) (cons x (funcall f))))

;; ;remove-duplicates
;; (lrec #'(lambda (x f) (adjoin x (funcall f))))

;; ;find-if, for some function fn
;; (lrec #'(lambda (x f) (if (fn x) x (funcall f))))

;; ;some, for some function fn
;; (lrec #'(lambda (x f) (or (fn x) (funcall f))))

(defun ttrav (rec &optional (base #'identity))
  "  Tree TRAVerser function generator:
  Returns a function that recurses a tree, when a leaf is encountered it
  applies/returns BASE and at each internal node REC is called with the first
  arg returned from recursing on the CAR and the second recursing on the CDR
  (or NIL if (CDR tree) = NIL).  The function returned always travels the
  entire tree"
  (declare (type (function (t t) t) rec))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base )
                 (funcall rec (self (car tree))
                          (if (cdr tree)
                              (self (cdr tree))
                              nil )))))
    #'self ))

(defun trec (rec &optional (base #'identity))
  "Tree RECursion function generator:
  Returns a function that accepts a tree and will, if an atom, return/call 
  BASE, and if not, call REC with arguments the tree and two functions
  which when called recurse down the CAR and CDR of the tree, respectively."
  (declare (type (function (list function function) t) rec))
  (labels 
    ((self (tree)
       (if (atom tree)
           (if (functionp base)
               (funcall base tree)
               base )
           (funcall rec tree
                    #'(lambda ()
                        (self (car tree)) )
                    #'(lambda ()
                        (if (cdr tree)
                            (self (cdr tree)) 
                            nil ))))))
    #'self ))

;; ;; Examples

;; ;; copy-tree
;; (ttrav #'cons)

;; ;; count-leaves
;; (let ((count-leaves (ttrav #'(lambda (l r) (+ l (or r 0))) 1)))
;;   (funcall count-leaves '(1 2 (3 4) ((4 5 (6) 7) (8 9)) 10)) )

;; ;; flatten
;; (let ((flatten (ttrav #'nconc #'mklist)))
;;   (funcall flatten '(1 2 3 (4 5))) )

;; ;; CONS will copy the tree, 1+ will add one to each element
;; (funcall (ttrav #'cons #'1+) '(1 2 3 (4 5)))

;; ;rfind-if for oddp
;; (let ((rfind-if-oddp (trec #'(lambda (o l r)
;;                                (declare (ignore o))
;;                                (or (funcall l) (funcall r)) )
;;                            #'(lambda (tree) (and (oddp tree) tree)) )))
;;   (funcall rfind-if-oddp '(2 4 (2 2 4 4 ((6) 8 (10) ((1)))) 3 5 7)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Macros returning functions

;; (with-compilation-unit (:override nil)
  (defmacro fn (expr)
    "Build a function like so:

     (fn (op func1 func2 ...)) => (lambda (x) (op (func1 x) (func2 x) ...))

   Compose is a special case:
     (fn (compose func1 func2 ... funcN))
        => (lambda (x) (func1 (func2 (... (funcN x)))))"
    `#',(rbuild expr) )

  (defun rbuild (expr)
    (if (or (atom expr) (eq (car expr) 'lambda))
        expr
        (if (eq (car expr) 'compose)
            (build-compose (cdr expr))
            (build-call (car expr) (cdr expr)) )))

  (defun build-call (op fns)
    (let ((g (gensym "BUILD-CALL-")))
      `(lambda (,g)
         (,op ,@(mapcar #'(lambda (f)
                            `(,(rbuild f) ,g) )
                        fns )))))

  (defun build-compose (fns)
    (let ((g (gensym "BUILD-COMPOSE-")))
      `(lambda (,g)
         ,(labels ((rec (fns)
                     (if fns
                         `(,(rbuild (car fns))
                            ,(rec (cdr fns)) )
                         g )))
            (rec fns) )))) ;; )

(defmacro alrec (rec &optional base)
  "Anaphoric List RECursor generator.  Return a function..."
  (let ((gfn (gensym "ALREC-")))
    `(lrec #'(lambda (it ,gfn)
               (symbol-macrolet ((rec (funcall ,gfn)))
                 ,rec ))
           ,base )))

(defmacro on-cdrs (rec base &rest lsts)
  "Recurse down LiSTS based on REC, returning BASE as the base case."
  `(funcall (alrec ,rec #'(lambda () ,base)) ,@lsts) )

(defmacro atrec (rec &optional (base 'it))
  (let ((lfn (gensym "ATREC-")) (rfn (gensym "ATREC-")))
    `(trec #'(lambda (it ,lfn ,rfn)
               (symbol-macrolet ((left (funcall ,lfn))
                                 (right (funcall ,rfn)) )
                 ,rec ))
           #'(lambda (it) ,base) )))

(defmacro on-trees (rec base &rest trees)
  `(funcall (atrec ,rec ,base) ,@trees) )

(defun find-all (el lst &optional (off 0))
  (cond ((null lst) nil)
        ((eql el (car lst)) (cons off (find-all el (cdr lst) (1+ off))))
        (t (find-all el (cdr lst) (1+ off))) ))

(defun shuffle-at (guide lst1 lst2 &optional (off 0))
  (cond ((or (null lst2) (null guide) (null lst1)) lst1)
        ((= off (car guide))
         (cons (car lst2) (shuffle-at (cdr guide) lst1 (cdr lst2) off)) )
        (t (cons (car lst1) (shuffle-at guide (cdr lst1) lst2 (1+ off)))) ))

(defun replace-at (guide lst1 lst2 &optional (off 0))
  (cond ((or (null lst2) (null guide) (null lst1)) lst1)
        ((= off (car guide))
         (cons (car lst2) (replace-at (cdr guide) (cdr lst1) (cdr lst2) (1+ off))) )
        (t (cons (car lst1) (replace-at guide (cdr lst1) lst2 (1+ off)))) ))

;;; Common Lisp Currying like behavior

;; (defun curry (fn &rest args)
;;   #'(lambda (&rest args2)
;;       (apply fn (append args args2)) ))

;; (defun rcurry (fn &rest args)
;;   #'(lambda (&rest args2)
;;       (apply fn (append args2 args)) ))

;;; Cute and Cut, similar to SRFI-26
(defun cute (&rest args)
  (lambda (&rest free-vars)
    (let* ((guide (find-all :<> args))
           (lst (replace-at guide args free-vars)))
      (apply (car lst) (append (cdr lst) (nthcdr (length guide) free-vars))) )))

;; (with-compilation-unit (:override nil)
  (defmacro cut ((fn &rest args))
    (let ((new-args (get-cut-params args)))
      `(let ,(remove-if (fun (compose #'quotep #'cadr) #'contains-cut-slot) new-args)
         (lambda ,(mapcar #'car (remove-if-not #'contains-cut-slot new-args))
           (funcall #',fn
                    ,@(mapcar #'(lambda (x)
                                  (cond ((quotep (cadr x))
                                         (cadadr x) )
                                        ((or (not (contains-cut-slot (cadr x)))
                                             (eql '<> (cadr x)) )
                                         (car x) )
                                        (t (replace-cut-slots (cadr x) (car x))) ))
                              new-args ))))))
 
  (defun quotep (x)
    (and (consp x) (eql 'quote (car x))) )
 
  (defun contains-cut-slot (tree)
    (funcall
      (ttrav #'(lambda (l r) (or l r))
             #'(lambda (x) (eql x '<>)) )
      tree ))
 
  (defun replace-cut-slots (tree binding)
    (funcall
      (ttrav #'(lambda (l r) (cons l r))
             #'(lambda (x) (cond ((eql x '<>) binding)
                                 (t x) )))
      tree ))
 
  (defun get-cut-params (args)
    (mapcar #'list
            (map-into
              (make-list (length args))
              #'(lambda () (gensym "CUT-")) )
            args )) ;; )

;; ;; Examples

;; ;;; You can use it to automate function definition, but then again
;; ;;; you shouldn't have to for the most part anymore.
;; (macroexpand-1 '(cut (+ 3 <>)))
;; (setf (symbol-function 'add3) (cut (+ 3 <>)))
;; (add3 4)

;; (funcall #.(compose #'sqrt (cut (/ 3 <>))) 323)

;; (mapcar (cut (list <> (list <> 3))) '(1 2 3) '(4 5 6))

;; (mapcar (compose #'sqrt (cut (/ 3 <>))) '(1 2 3 4 5))

;; ;;; Use quote to force evaluation of form every invocation (for 
;; ;;; non-functional code)
;; (macroexpand-1 '(cut (/ <> '(non-func 3 4))))

;; (with-open-file (f-in #p"scratch.lisp")
;;   (mapcar (cut (list <> (read f-in))) '(first second third)) )
;; (with-open-file (f-in #p"scratch.lisp")
;;   (mapcar (cut (list <> '(read f-in))) '(first second third)) )

;; (macroexpand-1 '(cut (func <> 2 (1+ <>) (+ 1 2) 3)))

;; (funcall (cut (identity (mvl (funcall <>)))) (cut (floor 1.5)))

;; ;; Examples

;; (funcall (cute #'+ 1) (+ 100 1) 2 3 3)

;; (funcall (cute (cute #'+ 1) 4))

;; (funcall (cute #'list 1 2) 4 5 )

;; (mapcar (cute :<> 3) '(sqrt - 1+))

;; (funcall (cute #'print :<>) (list 1 2 3))

;; ;; Examples

;; (macroexpand-1
;;   '(cut (list <> 1 (+ 1 1))) )

;; (macroexpand-1
;;   '(cut (list <> 1 2 3 <>)) )

;; (let ((a 5))
;;   (mapcar (cut (list a <> 1)) '(1 2 3 4)) )

;; (mapcar (cut (* <> (+ 1 1))) '(1 2 3 4))

;; (mapcar (compose (cut (cons 'hello <>)) #'list) '(how are you))

;; (funcall #.(cut (+ 1 pi 43 <>)) 2)

;; (defmacro t->argn (n form)
;;   (with-gensyms (ret "T->ARGN-")
;;     `(let ((,ret ,(nth n form)))
;;        (if ,form ,ret) )))

;; (defun permute-args (

;; (defmacro fill-in (val &body body)
;;   `(let ((__ ,val))
;;      (

;; (macroexpand-1 '(t->argn 1 (> 4 3)))

;; (let ((seq1 '(1 2 3 4 5))
;;       (seq2 '(4 3 2 1)) )
;;   (t->argn 2 (< (length seq1) (length seq2))) )

;;;;;;;;;;;;;;;;;;;
;;;; Classic macros

;;; Looping (these names conflict with every looping package in existence)
;; (defmacro while (test &body body)
;;   `(do ()
;;        ((not ,test))
;;      ,@body ))

;; (defmacro till (test &body body)
;;   `(do ()
;;        (,test)
;;      ,@body ))

;; (defmacro for ((var start stop) &body body)
;;   (let ((gstop (gensym "FOR-")))
;;     `(do ((,var ,start (1+ ,var))
;;           (,gstop ,stop) )
;;          ((> ,var ,gstop))
;;        ,@body )))

;;; conditional with automatic bind
(defmacro when-bind ((var expr) &body body)
  "Like when but with a binding to a variable 'var' (also see awhen)"
  `(let ((,var ,expr))
     (when ,var
       ,@body )))

(defmacro when-bind* (binds &body body)
  "Like when-bind but with multiple binds"
  (if (null binds)
    `(progn ,@body)
    `(let (,(car binds))
       (if ,(caar binds)
         (when-bind* ,(cdr binds) ,@body) ))))

;; ;; Examples

;; (when-bind (x 'a) x)

;; (when-bind* ((var1 (member 'a '(f g a b d)))
;;              (var2 (member 'd var1))
;;              (var3 (< 4 7)) )
;;   (values var1 var2 var3) )

(defmacro with-gensyms (syms &body body)
  "Like everybody elses with-gensyms, but with an optional prefix string
  as the last element."
  (cond ((stringp (last1 syms))
         `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(concatenate 'string
                                                                 (last1 syms)
                                                                 (string s) ))))
                        (remove-if #'stringp syms) )
            ,@body ))
        (t `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(string s))))
                          syms )
              ,@body ))))

(defun get-gensyms (n &optional (prefix "G"))
  "Return n new gensyms with given prefix."
  (cond ((= n 0) nil)
        (t (cons (gensym prefix) (get-gensyms (1- n) prefix))) ))

;; ;; Examples
;; (with-gensyms (a b c)
;;   (list a b c) )

;; (macroexpand-1
;;   '(with-gensyms (a b c "PRE-")
;;      (list a b c) ))

;; ;;; Adding a descriptive prefix to the generated names can aid in debugging...
;; (with-gensyms (gseq ret eof "PREFIX-")
;;   (list gseq ret eof) )

;; (with-compilation-unit (:override nil)
;;   (defmacro condlet (clauses &body body)
;;     "Conditional bindings"
;;     `(cond ,@(mapcar #'(lambda (cl)
;;                          `(,(car cl) (let ,(cdr cl) ,@body)) )
;;                      clauses ))) )

;; (with-compilation-unit (:override nil)
  (defmacro condlet (clauses &body body)
    "Conditional bindings"
    (let ((bodfn (gensym "CONDLET-"))
          (vars (mapcar #'(lambda (v) (cons v (gensym "CONDLET-")))
                        (remove-duplicates
                          (mapcar (lambda (x) (if (listp x) (car x) x))
                                  (mappend #'cdr clauses) )))))
      `(labels ((,bodfn ,(mapcar #'car vars)
                        ,@body ))
         (cond ,@(mapcar #'(lambda (cl)
                             (condlet-clause vars cl bodfn) )
                         clauses )))))

  (defun condlet-binds (vars cl)
    (mapcar #'(lambda (bindform)
                (if (consp bindform)
                    (cons (cdr (assoc (car bindform) vars))
                          (cdr bindform) )))
            (cdr cl) ))

  (defun condlet-clause (vars cl bodfn) ;; Modified to remove unecessary binds
    `(,(car cl) (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars)) )));; )

(defmacro if3 (test t-case nil-case ?-case)
  "Like if except allows for an ambiguous result if the predicate returns ?"
  `(case ,test
     ((nil) ,nil-case)
     (?     ,?-case)
     (t     ,t-case) ))

;; ;; Examples
;; ;;; This allows for an unsure altrnative if the first argument returns the 
;; ;;; symbol ?.  This could be used in cases where a trivalent branch is needed.
;; (macroexpand-1 '(if3 '? 't-case 'nil-case ?-case))

;; (if3 '? 't-case 'nil-case '?-case)

(defmacro nif (expr pos zero neg)
  "Numerical IF with clauses for positive, zero, and negative"
  (let ((g (gensym "NIF-")))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg) ))))

;; (defmacro in (obj &rest choices)
;;   "Member for the first argument in the rest of the arguments"
;;   (let ((insym (gensym "IN-")))
;;     `(let ((,insym ,obj))
;;        (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
;;                      choices )))))

(defmacro inq (obj &rest args)
  "Like in, but quote the rest of the arguments"
  `(in ,obj ,@(mapcar #'(lambda (a) `',a) args)) )

(defmacro in-if (fn &rest choices)
  "Like in, but with a predicate function rather than the implicit member"
  (let ((fnsym (gensym "IN-IF-")))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
                         `(funcall ,fnsym ,c) )
                     choices )))))

;; (with-compilation-unit (:override nil)
  (defmacro >case (expr &rest clauses)
    "Like case except each key is evaluated"
    (let ((g (gensym ">CASE-")))
      `(let ((,g ,expr))
         (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
                         clauses )))))

  (defun >casex (g cl)
    (let ((key (car cl)) (rest (cdr cl)))
      (cond ((consp key) `((in ,g ,@key) ,@rest))
            ((inq key t otherwise) `(t ,@rest))
            (t (error "bad > case clause")) )));;  )

;; ;; Examples

;; (nif 10 'p 'z 'n) ; p
;; (nif -0 'p 'z 'n) ; z
;; (nif -1 'p 'z 'n) ; n

;; (in 5 1 3 4 5) ; t
;; (in -1 5 4 23) ; nil
;; (in "hello" "hello" "howdy") ; The test function is eql

;; (inq 'a b c d a) ; t
;; (inq 'a x x y w) ; nil

;; (in-if #'integerp 1.5 2/3 4/4) ; t
;; (let ((x 6))
;;   (in-if #'oddp 2 2 4 4 x) )   ; nil: like (some #'oddp 2 2 4 4 6)
;; (in-if #'(lambda (y) (eql 5 y)) 5 4 3) ; t: like (member 5 (list 5 4 3) :test #'eql)

;; ;;; >case is a version of case that evaluates the keys (like 'a and 'b here)
;; (>case 'c
;;    (('a) 'hello)
;;    (('b) 'howdy)
;;    (t   'goodbye) )

(defmacro do-tuples/o (parms source &body body)
  (if parms
    (let ((src (gensym "DO-TUPLES/O-")))
      `(prog ((,src ,source))
         (mapc #'(lambda ,parms ,@body)
               ,@(map0-n #'(lambda (n)
                             `(nthcdr ,n ,src) )
                         (1- (length parms)) ))))))

(defun dt-args (len rest src)
  (map0-n #'(lambda (m)
              (map1-n #'(lambda (n)
                          (let ((x (+ m n)))
                            (if (>= x len)
                              `(nth ,(- x len) ,src)
                              `(nth ,(1- x) ,rest) )))
                      len ))
          (- len 2) ))

(defmacro do-tuples/c (parms source &body body)
  (if parms
    (with-gensyms (src rest bodfn "DO-TUPLES/C-")
      (let ((len (length parms)))
        `(let ((,src ,source))
           (when (nthcdr ,(1- len) ,src)
             (labels ((,bodfn ,parms ,@body))
               (do ((,rest ,src (cdr ,rest)))
                 ((not (nthcdr ,(1- len) ,rest))
                  ,@(mapcar #'(lambda (args)
                                `(,bodfn ,@args) )
                            (dt-args len rest src) )
                  nil )
                 (,bodfn ,@(map1-n #'(lambda (n)
                                       `(nth ,(1- n)
                                             ,rest ))
                                   len ))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Multiple value iteration

;; (with-compilation-unit (:override nil)
  (defmacro mvdo* (parm-cl test-cl &body body)
    (mvdo-gen parm-cl parm-cl test-cl body) )

  (defun mvdo-rebind-gen (rebinds)
    (cond ((null rebinds) nil)
        ((< (length (car rebinds)) 3)
         (mvdo-rebind-gen (cdr rebinds)) )
        (t (cons (list (if (atom (caar rebinds))
                           'setq
                           'multiple-value-setq )
                       (caar rebinds)
                       (third (car rebinds)) )
                 (mvdo-rebind-gen (cdr rebinds)) ))))

  (defun mvdo-gen (binds rebinds test body)
    (if (null binds)
        (let ((label (gensym "MVDO-GEN-")))
          `(prog nil
                 ,label
                 (if ,(car test)
                     (return (progn ,@(cdr test))) )
                 ,@body
                 ,@(mvdo-rebind-gen rebinds)
                 (go ,label) ))
        (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
          (let ((var/s (caar binds)) (expr (cadar binds)))
            (if (atom var/s)
              `(let ((,var/s ,expr)) ,rec)
              `(multiple-value-bind ,var/s ,expr ,rec) )))));; )

;; (with-compilation-unit (:override nil)
  (defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
         (syms  (mapcar #'(lambda (p)
                            (mapcar #'(lambda (x) 
                                        (declare (ignore x)) 
                                        (gensym "MVPSETQ-") )
                                    (mklist (car p)) ))
                        pairs )))
    (labels ((rec (ps ss)
               (if (null ps)
                   `(setq ,@(mapcan #'(lambda (p s)
                                        (shuffle (mklist (car p)) s))
                                    pairs syms ))
                   (let ((body (rec (cdr ps) (cdr ss))))
                     (let ((var/s (caar ps))
                           (expr (cadar ps)) )
                       (if (consp var/s)
                           `(multiple-value-bind ,(car ss) ,expr
                              ,body )
                           `(let ((,@(car ss) ,expr))
                              ,body )))))))
      (rec pairs syms) )))

  (defun shuffle (x y)
    "Interlaces two lists together (think of it as a zipper)"
    (cond ((null x) y)
          ((null y) x)
          (t (list* (car x) (car y)
                    (shuffle (cdr x) (cdr y)) )))) ;;)

(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym "MVDO-"))
        (temps (mapcar #'(lambda (b)
                           (if (listp (car b))
                             (mapcar #'(lambda (x)
                                         (declare (ignore x)) 
                                         (gensym "MVDO-") )
                                     (car b) )
                             (gensym "MVDO-") ))
                       binds )))
    `(let ,(mappend #'mklist temps)
       (mvpsetq ,@(mapcan #'(lambda (b var)
                              (list var (cadr b)) )
                          binds
                          temps ))
       (prog ,(mapcar #'(lambda (b var) (list b var))
                      (mappend #'mklist (mapcar #'car binds))
                      (mappend #'mklist temps) )
             ,label
             (if ,test
                 (return (progn ,@result)) )
             ,@body
             (mvqsetq ,@(mapcan #'(lambda (b)
                                    (if (third b)
                                        (list (car b)
                                              (third b) )))
                                binds ))
             (go ,label) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Generalized varaibles

(defmacro allf (val &rest args)
  "Set all places in ARGS to VAL."
  (with-gensyms (gval "ALLF-")
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
                       args )))))

(defmacro nilf (&rest args) "Set all ARGS to NIL" `(allf nil ,@args))

(defmacro tf (&rest args) "Set all ARGS to T" `(allf t ,@args))

(define-modify-macro toggle2 () not "Toggle the truthfulness of argument.")

(defmacro toggle (&rest args)
  "Toggle the truthfulness on each place in ARGS."
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle2 ,a))
               args )))

(define-modify-macro concf (obj) nconc)

(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj)) ))

(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
    (unless (apply #'member obj place args)
      (nconc place (list obj)) )))

(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
             (,(car var) (,op ,access ,@args)) )
       ,set )))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-expansion place)
    (let ((g (gensym "PULL-")))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)) )
         ,set ))))

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-expansion place)
    (let ((g (gensym "PULL-IF-")))
      `(let* ((,g ,test)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete-if ,g ,access ,@args)) )
         ,set ))))

(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
                       (get-setf-expansion place)
    (with-gensyms (gn glst "POPN-")
      `(let* ((,gn ,n)
              ,@(mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)) )
         (prog1 (subseq ,glst 0 ,gn)
                ,set )))))

(defmacro sortf (op &rest places)
  (let* ((meths (mapcar #'(lambda (p)
                            (multiple-value-list
                              (get-setf-expansion p) ))
                        places ))
         (temps (apply #'append (mapcar #'third meths))) )
    `(let* ,(mapcar #'list
                    (mapcan #'(lambda (m)
                                (append (first m)
                                        (third m) ))
                            meths )
                    (mapcan #'(lambda (m)
                                (append (second m)
                                        (list (fifth m)) ))
                            meths ))
       ,@(mapcon #'(lambda (rest)
                     (mapcar
                       #'(lambda (arg)
                           `(unless (,op ,(car rest) ,arg)
                              (rotatef ,(car rest) ,arg) ))
                       (cdr rest) ))
                 temps )
       ,@(mapcar #'fourth meths) )))

;; ;; Examples

;; (let ((x 1) (y 2) (z 3))
;;   (sortf < z y x)
;;   (list x y z) )

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Macro-defining macros

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args) ))

(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar #'(lambda (pair) `(abbrev ,@pair))
               (group names 2) )))

(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname) ))

(defmacro propmacros (&rest props)
  `(progn
     ,@(mapcar #'(lambda (p) `(propmacro ,p))
               props )))

;;;;;;;;;;;;;;;;
;;;; Read macros

;; ;;; Delimiter read macros
;; (let ((rpar (get-macro-character #\) )))
;;   (defun ddfn (left right fn)
;;     (set-macro-character right rpar)
;;     (set-dispatch-macro-character #\# left
;;       #'(lambda (stream char1 char2)
;;           (declare (ignore char1 char2))
;;           (apply fn (read-delimited-list right stream t)) ))))

;; (defmacro defdelim (left right parms &body body)
;;   `(ddfn ,left ,right #'(lambda ,parms ,@body)) )

;; (defdelim #\[ #\] (x y)
;;   (list 'quote (mapa-b #'identity (ceiling x) (floor y))) )

;; (defdelim #\{ #\} (&rest args)
;;   `(fn (compose ,@args)) )

;; ;; Examples

;; #[2 7]

;; #{1+ car}
;; (funcall * '(1d0 2 3))

;;; A read macro to silence output
(set-dispatch-macro-character #\# #\!
  #'(lambda (stream char1 char2)
      (declare (ignore char1 char2))
      `(progn ,(read stream t nil t) (values)) ))

;; ;; Examples

;; #!(setf *list* (make-list 1000000))

;; *list* ; console flooded with output

;; (unintern '*list*)

;; (cl-user:gc :full t)
