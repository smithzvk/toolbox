;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; My ``On Lisp'' based tool box ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Utility functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :toolbox)

;;; List utilities
(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)) )

(defun single (lst)
  (and (consp lst) (not (cdr lst))) )

(defun append1 (lst obj)
  (append lst (list obj)) )

(defun conc1 (lst obj)
  (nconc lst (list obj)) )

(defun mklist (obj)
  (if (listp obj) obj (list obj)) )

#| Examples
(last '(1 2 3 4))
(last1 '(1 2 3 4))

(single '(1))
(single '(1 2))

(append1 '(1 2 3 4) 5)

(mklist '(5))
(mklist 5)

|#

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

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))) )))
    (rec x nil) ))

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

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test)) ))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test)) ))

#| Examples

;;; Similar to find-if, member-if but returns the evaluation as the 
;;; second argument
(find2 #'evenp '(1 2 3 4 5))
(find2 (cut (member 5 <>)) '((1 2 3) (2 7 6 3) (3 5 3) (4) (5 9 2)))
(find-if #'evenp '(1 2 3 4 5))
(member-if #'evenp '(1 2 3 4 5))

;;; Is the first occurence of arg1 before/after the first occurence of arg2?
(before 'a 'b '(1 2 3 b 5 a))
(before 'a 'b '(1 a 2 3 b 5 a))

(after 'a 'b '(1 2 3 b 5 a))
(after 'a 'b '(1 a 2 3 b 5 a))

|#

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test ))

(defun split-on (fn lst)
  (unless (null lst)
    (multiple-value-bind (on off) (split-on fn (cdr lst))
      (if (funcall fn (car lst))
          (values (cons (car lst) on) off)
          (values on (cons (car lst) off)) ))))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
      ((or (null src) (funcall fn (car src)))
       (values (nreverse acc) src) )
      (push (car src) acc) )))

#| Examples

(duplicate 1 '(1 2 1 3))
(duplicate 2 '(1 2 1 3))

(split-on #'oddp '(1 2 3 4))

(split-if #'oddp '(2 2 3 4))

(split-if (cut (> <> 4)) '(1 2 3 4 5 6 7 8))

|#

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

#| Examples

(most #'length '((a b) (c d e) (f) (g h i)))

(best #'> '(1 2 3 4 5 3 2))

(mostn #'length '((a b) (a b c) (a) (e f g)))

|#

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

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)) )

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

#| Examples

(map0-n #'1+ 5)

; Can only map unary functions over the range)
(mapa-b #'identity 1 8)

(mapa-b #'+ 1 4 0.2d0)

(map-> #'/              ; function to map on (once again unary only)
       1                ; start value
       #.(cut (> <> 3)) ; exit clause
       #'1+ )           ; next element

; Not really sure what this is good for
(mappend #'identity '((1 3 4) (2 10 11) (3 2 1) (4 5 6)))

; Cool stuff you can do with cut
(mapcar (cut (apply #'+ <>)) '((1 2 3) (4 5 6)))
(mapcar (cut (reduce #'+ <>)) '((1 2 3) (4 5 6)))

;Series code, doesn't seem to work?
;(collect (#Mfn (scan-range :from a :upto b :by c)))

; Mapping over several lists without consing
(mapcars #'sqrt '(1 2 3 4) '(4 5 6 7)) ; == (mapcar #'sqrt (append '(1 2 3 4) '(4 5 6 7)))

; Recursive mapcar
(rmapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40)))

|#

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

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))) )

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))) )

(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1 :initial-element c)) )
       (symbol-name sym) ))

#| Examples

(mkstr 'hello)
(mkstr 1984)
(mkstr '(1 2 3 4))
(mkstr (make-hash-table))
(string 'howdy)
;(string 1984)       ;error
;(string '(1 2 3 4)) ;error
(symbol-name 'heck)

;;; Largely useless...
(symb 'hello)
(type-of (symb 'hello))

(reread '(1 2 3 4))

(explode 'hello)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Function returning functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun memoize (fn &key (size nil size-p) (test #'equalp))
  "Function memoizer"
  (let ((cache (if size-p
                   (make-hash-table :size size :test test)
                   (make-hash-table :test test)) ))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args) ))))))

#| Examples

;;;See string-algs.lisp for dynamic programming examples

(defun slow-func (x)
  (sleep 2)
  x )

(setf (symbol-function 'slow-func) (memoize #'slow-func))

(slow-func 1)
(slow-func 2)

(slow-func 1) ; The sleep is not executed, it just remembers the answer and returns it
(slow-func 2)

|#

(defun compose (&rest fns)
  "Function compose"
  (if fns
    (let ((fn1 (last1 fns))
          (fns (butlast fns)) )
      #'(lambda (&rest args)
          (reduce #'funcall fns
                  :from-end t
                  :initial-value (apply fn1 args) )))
    #'identity ))

(defun fif (if then &optional else)
  "Functional if"
  #'(lambda (x)
      (if (funcall if x)
        (funcall then x)
        (if else (funcall else x)) )))

;;; Function intersection
(defun fint (fn &rest fns)
  "Function INTersection"
  (if (null fns)
    fn
    (let ((chain (apply #'fint fns)))
      #'(lambda (x)
          (and (funcall fn x) (funcall chain x)) ))))

;;; Function union
(defun fun (fn &rest fns)
  "Function UNion"
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x)) ))))

#| Examples

(mapcar (compose (cut (list 'begin <> 'end)) #'1+) '(1 2 3 4 5))

(mapcar (compose #'sqrt ; takes 1 argument
                 #'/ )  ; takes 2 arguments
        '(1 2 3 4 5) '(5 4 3 2 1) )

; Move up to the next even integer
(funcall (fif #'oddp #'1+ #'identity) 4)

; Integers which are odd and even (i.e. non of them)
(mapcar (fint #'oddp #'evenp) '(1 2 3 4))

; Integers which are odd or even (i.e. all of them)
(mapcar (fun #'oddp #'evenp) '(1 2 3 4))

|#

(defun lrec (rec &optional base)
  "List RECursion function generator"
  (labels ((self (lst)
             (if (null lst)
               (if (functionp base)
                 (funcall base)
                 base )
               (funcall rec (car lst)
                        #'(lambda ()
                            (self (cdr lst)) )))))
    #'self ))

#| Examples

;length
(lrec #'(lambda (x f) (1+ (funcall f))) 0)

;every (for oddp)
(lrec #'(lambda (x f) (and (oddp x) (funcall f))) t)

;copy-list
(lrec #'(lambda (x f) (cons x (funcall f))))

;remove-duplicates
(lrec #'(lambda (x f) (adjoin x (funcall f))))

;find-if, for some function fn
(lrec #'(lambda (x f) (if (fn x) x (funcall f))))

;some, for some function fn
(lrec #'(lambda (x f) (or (fn x) (funcall f))))

|#

(defun ttrav (rec &optional (base #'identity))
  "Tree TRAVerser function generator"
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base )
                 (funcall rec (self (car tree))
                          (if (cdr tree)
                              (self (cdr tree)) )))))
    #'self ))

(defun trec (rec &optional (base #'identity))
  "Tree RECursion function generator"
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
                        (self (cdr tree)) ))))))
    #'self ))

#| Examples

;copy-tree
(ttrav #'cons)

;count-leaves
(ttrav #'(lambda (l r) (+ l (or r 1))) 1)

;flatten
(ttrav #'nconc #'mklist)

(funcall * '(1 2 3 (4 5)))

(funcall (ttrav #'cons #'1+) '(1 2 3 (4 5)))

;flatten
(trec #'(lambda (o l r) (or (funcall l) (funcall r)))
      #'(lambda (tree) (and (oddp tree) tree)) )

;rfind-if for oddp
(trec #'(lambda (o l r) (or (funcall l) (funcall r)))
      #'(lambda (tree) (and (oddp tree) tree)) )

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Macros returning functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-compilation-unit (:override nil)
  (defmacro fn (expr)
    ""
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
            (rec fns) ))))
  )

(defmacro alrec (rec &optional base)
  (let ((gfn (gensym "ALREC-")))
    `(lrec #'(lambda (it ,gfn)
               (symbol-macrolet ((rec (funcall ,gfn)))
                 ,rec ))
           ,base )))

(defmacro on-cdrs (rec base &rest lsts)
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

;;; Cut (Common Lisp Currying like behavior)
(with-compilation-unit (:override nil)
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
            args ))
  )

#| Examples

;;; You can use it to automate function definition, but then again
;;; you shouldn't have to for the most part anymore.
(macroexpand-1 '(cut (+ 3 <>)))
(setf (symbol-function 'add3) (cut (+ 3 <>)))
(add3 4)

(funcall #.(compose #'sqrt (cut (/ 3 <>))) 323)

(mapcar (cut (list <> (list <> 3))) '(1 2 3) '(4 5 6))

(mapcar (compose #'sqrt (cut (/ 3 <>))) '(1 2 3 4 5))

;;; Use quote to force evaluation of form every invocation (for 
;;; non-functional code)
(macroexpand-1 '(cut (/ <> '(non-func 3 4))))

(with-open-file (f-in #p"scratch.lisp")
  (mapcar (cut (list <> (read f-in))) '(first second third)) )
(with-open-file (f-in #p"scratch.lisp")
  (mapcar (cut (list <> '(read f-in))) '(first second third)) )

(macroexpand-1 '(cut (func <> 2 (1+ <>) (+ 1 2) 3)))

(funcall (cut (identity (mvl (funcall <>)))) (cut (floor 1.5)))

|#

#| Examples

(macroexpand-1
  '(cut (list <> 1 (+ 1 1))) )

(macroexpand-1
  '(cut (list <> 1 2 3 <>)) )

(let ((a 5))
  (mapcar (cut (list a <> 1)) '(1 2 3 4)) )

(mapcar (cut (* <> (+ 1 1))) '(1 2 3 4))

(mapcar (compose (cut (cons 'hello <>)) #'list) '(how are you))

(funcall #.(cut (+ 1 pi 43 <>)) 2)

|#

#|

(defmacro t->argn (n form)
  (with-gensyms (ret "T->ARGN-")
    `(let ((,ret ,(nth n form)))
       (if ,form ,ret) )))

(defun permute-args (

(defmacro fill-in (val &body body)
  `(let ((__ ,val))
     (

(macroexpand-1 '(t->argn 1 (> 4 3)))

(let ((seq1 '(1 2 3 4 5))
      (seq2 '(4 3 2 1)) )
  (t->argn 2 (< (length seq1) (length seq2))) )

|#

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lazy evaluation ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant unforced (gensym "UNFORCED-"))

(defstruct delay forced closure)

(defmacro delay (expr)
  (let ((self (gensym "DELAY-")))
    `(let ((,self (make-delay :forced unforced)))
       (setf (delay-closure ,self)
             #'(lambda ()
                 (setf (delay-forced ,self) ,expr) ))
       ,self )))

(defun force (x)
  (if (delay-p x)
      (if (eq (delay-forced x) unforced)
          (funcall (delay-closure x))
          (delay-forced x) )
      x ))

#| Examples

(delay (+ 1 1))

(force (delay (+ 1 1)))

(defun sinc (x)
  (let ((arg (delay (/ (sin x) x))))
    (if (not (= 0 x))
        (force arg)
        1 )))

(map-into (make-array 10) #'sinc (mapa-b #'identity 0 2 .2))

;;; Delaying input to make a function based if
(defun if-func (pred then else)
  (if (force pred)
      (force then)
      (force else) ))

(if-func t   (delay (print 'then)) (delay (print 'else)))
(if-func nil (delay (print 'then)) (delay (print 'else)))

|#

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Classic macros ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;; Looping (these names conflict with every looping package in existence)
(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body ))

(defmacro till (test &body body)
  `(do ()
     (,test)
     ,@body ))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym "FOR-")))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop) )
       ((> ,var ,gstop))
       ,@body )))

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

#| Examples

(when-bind (x 'a) x)

(when-bind* ((var1 (member 'a '(f g a b d)))
             (var2 (member 'd var1))
             (var3 (< 4 7)) )
  (values var1 var2 var3) )

|#

(defmacro with-gensyms (syms &body body)
  (cond ((stringp (last1 syms))
         `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(concatenate 'string
                                                                 (last1 syms)
                                                                 (string s) ))))
                        (remove-if #'stringp syms) )
            ,@body ))
        (t `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(string s))))
                          syms )
              ,@body ))))

#| Examples
(with-gensyms (a b c)
  (list a b c) )

(macroexpand-1
  '(with-gensyms (a b c "PRE-")
     (list a b c) ))

;;; Adding a descriptive prefix to the generated names can aid in debugging...
(with-gensyms (gseq ret eof "PREFIX-")
  (list gseq ret eof) )

|#

(with-compilation-unit (:override nil)
  (defmacro condlet (clauses &body body)
    "Conditional bindings"
    (let ((bodfn (gensym "CONDLET-"))
          (vars (mapcar #'(lambda (v) (cons v (gensym "CONDLET-")))
                        (remove-duplicates
                          (mapcar #'car
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
                  (,bodfn ,@(mapcar #'cdr vars)) )))
  )

(defmacro if3 (test t-case nil-case ?-case)
  "Like if except allows for an ambiguous result if the predicate returns ?"
  `(case ,test
     ((nil) ,nil-case)
     (?     ,?-case)
     (t     ,t-case) ))

#| Examples
;;; This allows for an unsure altrnative if the first argument returns the 
;;; symbol ?.  This could be used in cases where a trivalent branch is needed.
(macroexpand-1 '(if3 '? 't-case 'nil-case ?-case))

(if3 '? 't-case 'nil-case '?-case)

|#

(defmacro nif (expr pos zero neg)
  "Numerical if with clauses for positive, zero, and negative"
  (let ((g (gensym "NIF-")))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg) ))))

(defmacro in (obj &rest choices)
  "Member for the first argument in the rest of the arguments"
  (let ((insym (gensym "IN-")))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices )))))

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

(with-compilation-unit (:override nil)
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
            (t (error "bad > case clause")) )))
  )

#| Examples

(nif 10 'p 'z 'n) ; p
(nif -0 'p 'z 'n) ; z
(nif -1 'p 'z 'n) ; n

(in 5 1 3 4 5) ; t
(in -1 5 4 23) ; nil
(in "hello" "hello" "howdy") ; The test function is eql

(inq 'a b c d a) ; t
(inq 'a x x y w) ; nil

(in-if #'integerp 1.5 2/3 4/4) ; t
(let ((x 6))
  (in-if #'oddp 2 2 4 4 x) )   ; nil: like (some #'oddp 2 2 4 4 6)
(in-if #'(lambda (y) (eql 5 y)) 5 4 3) ; t: like (member 5 (list 5 4 3) :test #'eql)

;;; >case is a version of case that evaluates the keys (like 'a and 'b here)
(>case 'c
   (('a) 'hello)
   (('b) 'howdy)
   (t   'goodbye) )

|#

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Multiple value iteration ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-compilation-unit (:override nil)
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
              `(multiple-value-bind ,var/s ,expr ,rec) ))))) )

(with-compilation-unit (:override nil)
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
                    (shuffle (cdr x) (cdr y)) )))) )

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

#| Examples

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Generalized varaibles ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro allf (val &rest args)
  (with-gensyms (gval "ALLF-")
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
                       args )))))

(defmacro nilf (&rest args) `(allf nil ,@args))

(defmacro tf (&rest args) `(allf t ,@args))

(define-modify-macro toggle2 () not)

(defmacro toggle (&rest args)
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

#| Examples

(let ((x 1) (y 2) (z 3))
  (sortf < z y x)
  (list x y z) )

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Macro-defining macros ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;
;;;; Read macros ;;;;
;;;;;;;;;;;;;;;;;;;;;

;;; Delimiter read macros
(let ((rpar (get-macro-character #\) )))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
      #'(lambda (stream char1 char2)
          (declare (ignore char1 char2))
          (apply fn (read-delimited-list right stream t)) ))))

(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)) )

(defdelim #\[ #\] (x y)
  (list 'quote (mapa-b #'identity (ceiling x) (floor y))) )

(defdelim #\{ #\} (&rest args)
  `(fn (compose ,@args)) )

#| Examples

#[2 7]

#{1+ car}
(funcall * '(1d0 2 3))

|#

;;; A read macro to silence output
(set-dispatch-macro-character #\# #\!
  #'(lambda (stream char1 char2)
      (declare (ignore char1 char2))
      `(progn ,(read stream t nil t) 'output-suppressed) ))

#| Examples

#!(make-list 1000000)

|#

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

;;;;;;;;;;;;;;;;;;;;;;;
;;;; Continuations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(setq *cont* #'identity)

(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body) )

(defmacro =defun (name parms &body body)
  (let ((f (intern (concatenate 'string "=" (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,',f *cont* ,,@parms) )
       (defun ,f (*cont* ,@parms) ,@body) )))

(defmacro =bind (parms expr &body body)
  `(let ((*cont* #'(lambda ,parms ,@body))) ,expr) )

(defmacro =values (&rest retvals)
  `(funcall *cont* ,@retvals) )

(defmacro =funcall (fn &rest args)
  `(funcall ,fn *cont* ,@args) )

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args) )

#| Examples

(macroexpand-1
  '(=defun add1 (x) (=values (1+ x))) )

(defun dft (tree)
  (cond ((null tree) nil)
        ((atom tree) (princ tree))
        (t (dft (car tree))
           (dft (cdr tree)) )))

(setq *saved* nil)

(=defun dft-node (tree)
  (cond ((null tree) (restrt))
        ((atom tree) (=values tree))
        (t (push #'(lambda () (dft-node (cdr tree)))
                 *saved* )
           (dft-node (car tree)) )))

(=defun restrt ()
  (if *saved*
    (funcall (pop *saved*))
    (=values 'done) ))

(=defun dft2 (tree)
  (setq *saved* nil)
  (=bind (node) (dft-node tree)
    (cond ((eq node 'done) (=values nil))
          (t (princ node)
             (restrt) ))))

(let ((t1 '(a (b (d c)) (c e (f i) g)))
      (t2 '(1 (2 (3 8 7) 4 5))) )
  (dft2 t1)
  (dft2 t2) )

|#

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Multiprocessing ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct proc pri state wait)

(proclaim '(special *procs* *proc*))

(defvar *halt* (gensym))

(defvar *default-proc*
  (make-proc :state #'(lambda (x)
                        (format t "~%>> ")
                        (princ (eval (read)))
                        (pick-process) )))

(defmacro fork (expr pri)
  `(prog1 ',expr
     (push (make-proc
             :state #'(lambda (,(gensym "FORK-"))
                        ,expr
                        (pick-process) )
             :pri   ,pri )
           *procs* )))

(defmacro program (name args &body body)
  `(=defun ,name ,args
     (setq *procs* nil)
     ,@body
     (catch *halt* (loop (pick-process))) ))

(defun pick-process ()
  (multiple-value-bind (p val) (most-urgent-process)
    (setq *proc* p
          *procs* (delete p *procs*) )
    (funcall (proc-state p) val) ))

(defun most-urgent-process ()
  (let ((proc1 *default-proc*) (max -1) (val1 t))
    (dolist (p *procs*)
      (let ((pri (proc-pri p)))
        (if (> pri max)
          (let ((val (or (not (proc-wait p))
                         (funcall (proc-wait p)) )))
            (when val
              (setq proc1 p
                    max pri
                    val1 val ))))))
    (values proc1 val1) ))

(defun arbitrator (test cont)
  (setf (proc-state *proc*) cont
        (proc-wait *proc*) test )
  (push *proc* *procs*)
  (pick-process) )

(defmacro wait (parm test &body body)
  `(arbitrator #'(lambda () ,test)
               #'(lambda (,parm) ,@body) ))

(defmacro yield (&body body)
  `(arbitrator nil #'(lambda (,(gensym "YIELD-")) ,@body)) )

(defun setpri (n) (setf (proc-pri *proc*) n))

(defun halt (&optional val) (throw *halt* val))

(defun kill (&optional obj &rest args)
  (if obj
      (setq *procs* (apply #'delete obj *procs* args))
      (pick-process) ))

#| Examples

(defvar *open-doors* nil)

(=defun pedestrian ()
  (wait d (car *open-doors*)
        (format t "Entering ~A~%" d) ))

(program ped ()
  (fork (pedestrian) 1) )

(ped)

(defvar *bboard* nil)

(defun claim (&rest f) (push f *bboard*))

(defun unclaim (&rest f) (pull f *bboard* :test #'equal))

(defun check (&rest f) (find f *bboard* :test #'equal))

(=defun visitor (door)
  (format t "Approach ~A. " door)
  (claim 'knock door)
  (wait d (check 'open door)
    (format t "Enter ~A. " door)
    (unclaim 'knock door)
    (claim 'inside door) ))

(=defun host (door)
  (wait k (check 'knock door)
    (format t "Open ~A. " door)
    (claim 'open door)
    (wait g (check 'inside door)
      (format t "Close ~A.~%" door)
      (unclaim 'open door) )))

(program ballet ()
  (fork (visitor 'door1) 1)
  (fork (host 'door1) 1)
  (fork (visitor 'door2) 1)
  (fork (host 'door2) 1) )

(ballet)

(=defun capture (city)
  (take city)
  (setpri 1)
  (yield
    (fortify city) ))

(=defun plunder (city)
  (loot city)
  (ransom city) )

(defun take (c)    (format t "Liberating ~A.~%" c))
(defun fortify (c) (format t "Rebuilding ~A.~%" c))
(defun loot (c)    (format t "Nationalizing ~A.~%" c))
(defun ransom (c)  (format t "Refinancing ~A.~%" c))

(program barbarians ()
  (fork (capture 'rome) 100)
  (fork (plunder 'rome) 98) )

(barbarians)

|#

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Nondeterminism ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *paths* nil)
(defconstant failsym '@)

(defmacro choose (&rest choices)
  (if choices
      `(progn
         ,@(mapcar #'(lambda (c)
                       `(push #'(lambda () ,c) *paths*) )
                   (reverse (cdr choices)) )
         ,(car choices) )
      '(fail) ))

(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices) )

(defun cb (fn choices)
  (if choices
    (progn
      (if (cdr choices)
          (push #'(lambda () (cb fn (cdr choices)))
                *paths* ))
      (funcall fn (car choices)) )
    (fail) ))

(defun fail ()
  (if *paths*
      (funcall (pop *paths*))
      failsym ))

#| Examples

(defun do2 (x)
  (choose (+ x 2) (* x 2) (expt x 2)) )

(do2 3)

(fail)
(fail)
(fail)

(choose-bind x '(marrakesh strambourg vegas)
  (format nil "Let's go to ~A." x) )

(fail)(fail)(fail)

(=defun parlor-trick (sum)
  (=bind (n1 n2) (two-numbers)
    (if (= (+ n1 n2) sum)
        `(the sum of ,n1 ,n2)
        (fail) )))

(=defun two-numbers ()
  (choose-bind n1 '(0 1 2 3 4 5)
    (choose-bind n2 '(0 1 2 3 4 5)
      (=values n1 n2) )))

(parlor-trick 5)

|#

;;;;;;;;;;;;;
;;;; ATN ;;;;
;;;;;;;;;;;;;

(defmacro defnode (name &rest arcs)
  `(=defun ,name (pos regs) (choose ,@arcs)) )

(defmacro down (sub next &rest cmds)
  `(=bind (* pos regs) (,sub pos (cons nil regs))
     (,next pos ,(compile-cmds cmds)) ))

(defmacro cat (cat next &rest cmds)
  `(if (= (length *sent*) pos)
       (fail)
       (let ((* (nth pos *sent*)))
         (if (member ',cat (types *))
             (,next (1+ pos) ,(compile-cmds cmds))
             (fail) ))))

(defmacro jump (next &rest cmds)
  `(,next pos ,(compile-cmds cmds)) )

(defun compile-cmds (cmds)
  (if (null cmds)
      'regs
      `(,@(car cmds) ,(compile-cmds (cdr cmds))) ))

(defmacro up (expr)
  `(let ((* (nth pos *sent*)))
     (=values ,expr pos (cdr regs)) ))

(defmacro getr (key &optional (regs 'regs))
  `(let ((result (cdr (assoc ',key (car ,regs)))))
     (if (cdr result) result (car result)) ))

(defmacro set-register (key val regs)
  `(cons (cons (cons ,key ,val) (car ,regs))
         (cdr ,regs) ))

(defmacro setr (key val regs)
  `(set-register ',key (list ,val) ,regs) )

(defmacro pushr (key val regs)
  `(set-register ',key
                 (cons ,val (cdr (assoc ',key (car ,regs))))
                 ,regs ))

(defmacro with-parses (node sent &body body)
  (with-gensyms (pos regs "WITH-PARSES-")
    `(progn
       (setq *sent* ,sent)
       (setq *paths* nil)
       (=bind (parse ,pos ,regs) (,node 0 '(nil))
         (if (= ,pos (length *sent*))
             (progn ,@body (fail))
             (fail) )))))

#| Examples

(compile-cmds '((setr a b) (setr c d)))

(defun types (w)
  (cdr (assoc w '((spot noun) (runs verb)))) )

(with-parses s '(spot runs)
  (format t "Parsing: ~A~%" parse) )

(defun types (word)
  (case word
    ((do does did) '(aux v))
    ((time times) '(n v))
    ((fly flies) '(n v))
    ((like) '(v prep))
    ((liked likes) '(v))
    ((a an the) '(det))
    ((arrow arrows) '(n))
    ((i you he she him her it) '(pron)) ))

(defnode mods
  (cat n mods/n
    (setr mods *) ))

(defnode mods/n
  (cat n mods/n
    (pushr mods *) )
  (up `(n-group ,(getr mods))) )

(defnode np
  (cat det np/det
    (setr det *) )
  (jump np/det
    (setr det nil) )
  (cat pron pron
    (setr n *) ))

(defnode pron
  (up `(np (pronoun ,(getr n)))) )

(defnode np/det
  (down mods np/mods
    (setr mods *) )
  (jump np/mods
    (setr mods nil) ))

(defnode np/mods
  (cat n np/n
    (setr n *) ))

(defnode np/n
  (up `(np (det ,(getr det))
           (modifiers ,(getr nods))
           (noun ,(getr n)) ))
  (down pp np/pp
    (setr pp *) ))

(defnode np/pp
  (up `(np (det ,(getr det))
           (modifiers ,(getr mods))
           (noun ,(getr n))
           ,(getr pp) )))

(defnode pp
  (cat prep pp/prep
    (setr prep *) ))

(defnode pp/prep
  (down np pp/np
    (setr op *) ))

(defnode pp/np
  (up `(pp (prep ,(getr prep))
           (obj ,(getr op)) )))

(defnode s
  (down np s/subj
    (setr mood 'decl)
    (setr subj *) )
  (cat v v
    (setr mood 'imp)
    (setr subj '(np (pron you)))
    (setr aux nil)
    (setr v *) ))

(defnode s/subj
  (cat v v
    (setr aux nil)
    (setr v *) ))

(defnode v
  (up `(s (mood ,(getr mood))
          (subj ,(getr subj))
          (vcl (aux ,(getr aux))
               (v ,(getr v)) )))
  (down np s/obj
    (setr obj *) ))

(defnode s/obj
  (up `(s (mood ,(getr mood))
          (subj ,(getr subj))
          (vcl (aux ,(getr aux))
               (v ,(getr v)) )
          (obj ,(getr obj)) )))


(with-parses np '(a time fly like him)
  (pprint parse) )

(with-parses s '(time flies like an arrow)
  (pprint parse) )

1

|#

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Query Compiler ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-db (&optional (size 100))
  (make-hash-table :size size) )

(defvar *default-db* (make-db))

(defun clear-db (&optional (db *default-db*))
  (clrhash db) )

(defmacro db-query (key &optional (db '*default-db*))
  `(gethash ,key ,db) )

(defun db-push (key val &optional (db *default-db*))
  (push val (db-query key db)) )

(defmacro fact (pred &rest args)
  `(progn (db-push ',pred ',args)
          ',args ))

#| An interpreter implementation

(defmacro do-answers (query &body body)
  (let ((binds (gensym "DO-ANSWERS-")))
    `(dolist (,binds (interpret-query ',query))
       (let ,(mapcar #'(lambda (v)
                         `(,v (binding ',v ,binds)) )
                     (vars-in query #'atom) )
         ,@body ))))

(defun interpret-query (expr &optional binds)
  (case (car expr)
    (and (interpret-and (reverse (cdr expr)) binds))
    (or  (interpret-or (cdr expr) binds))
    (not (interpret-not (cadr expr) binds))
    (t   (lookup (car expr) (cdr expr) binds)) ))

(defun interpret-and (clauses binds)
  (if (null clauses)
      (list binds)
      (mapcan #'(lambda (b)
                  (interpret-query (car clauses) b) )
              (interpret-and (cdr clauses) binds) )))

(defun interpret-or (clauses binds)
  (mapcan #'(lambda (c)
              (interpret-query c binds) )
          clauses ))

(defun interpret-not (clause binds)
  (if (interpret-query clause binds)
      nil
      (list binds) ))

(defun lookup (pred args &optional binds)
  (mapcan #'(lambda (x)
              (aif2 (match x args binds) (list it)) )
          (db-query pred) ))

|#

;;; Compiled implementation

(with-compilation-unit (:override nil)

  (defmacro do-answers (query &body body)
    `(with-gensyms ,(append1 (vars-in query #'simple?) "DO-ANSWERS-")
       ,(compile-query query `(progn ,@body)) ))

  (defun compile-query (q body)
    (case (car q)
      (and  (compile-and (cdr q) body))
      (or   (compile-or  (cdr q) body))
      (not  (compile-not (cadr q) body))
      (lisp `(if ,(cadr q) ,body))
      (t    (compile-simple q body)) ))

  (defun compile-simple (q body)
    (let ((fact (gensym "COMPILE-SIMPLE-")))
      `(dolist (,fact (db-query ',(car q)))
         (pat-match ,(cdr q) ,fact ,body nil) )))

  (defun compile-and (clauses body)
    (if (null clauses)
        body
        (compile-query (car clauses)
                       (compile-and (cdr clauses) body) )))

  (defun compile-or (clauses body)
    (if (null clauses)
        nil
        (let ((gbod (gensym "COMPILE-OR-"))
              (vars (vars-in body #'simple?)) )
          `(labels ((,gbod ,vars ,body))
             ,@(mapcar #'(lambda (cl)
                           (compile-query cl `(,gbod ,@vars)) )
                       clauses )))))

  (defun compile-not (q body)
    (let ((tag (gensym "COMPILE-NOT-")))
      `(if (block ,tag
             ,(compile-query q `(return-from ,tag nil))
             t )
           ,body ))) )

#| Examples

(clear-db)
(fact painter hogarth william english)
(fact painter canale antonio venetian)
(fact painter reynolds joshua english)
(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)

(do-answers (painter hogarth ?x ?y)
  (princ (list ?x ?y)) )

(do-answers (and (painter ?x _ _)
                  (dates ?x 1697 _) )
  (princ (list ?x)) )

(do-answers (and (painter ?x _ _)
                  (dates ?x _ ?d)
                  (lisp (< 1770 ?d 1800)) )
  (princ (list ?x ?d)) )

|#

