;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; My ``On Lisp'' based tool box ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :toolbox)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Lazy evaluation ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel)
(defconstant +unforced+ (gensym "UNFORCED-")) )

(defstruct delay forced closure)

(defmacro delay (expr)
  (let ((self (gensym "DELAY-")))
    `(let ((,self (make-delay :forced +unforced+)))
       (setf (delay-closure ,self)
             #'(lambda ()
                 (setf (delay-forced ,self) ,expr) ))
       ,self )))

(defun force (x)
  (if (delay-p x)
      (if (eq (delay-forced x) +unforced+)
          (funcall (delay-closure x))
          (delay-forced x) )
      x ))

#|| Examples

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

||#

;;;;;;;;;;;;;;;;;;;;;;;
;;;; Continuations ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(setq *cont* #'identity)

(eval-when (:compile-toplevel :load-toplevel :execute)

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
  `(apply ,fn *cont* ,@args) ) )

#|| Examples

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

||#

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

#|| Examples

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

||#

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Nondeterminism ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *paths* nil)
(defconstant failsym '@)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro choose (&rest choices)
  (if choices
      `(progn
         ,@(mapcar #'(lambda (c)
                       `(push #'(lambda () ,c) *paths*) )
                   (reverse (cdr choices)) )
         ,(car choices) )
      '(fail) ))

(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices) ) )

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

#|| Examples

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

||#

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

#|| Examples

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

||#

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

#|| An interpreter implementation

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

||#

;;; Compiled implementation

;; (with-compilation-unit (:override nil)

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
           ,body ))) ;; )

#|| Examples

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

||#

