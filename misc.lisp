
(in-package :toolbox)

;;;; BY-ELTS : cons-free, implicit iteration over vectors
;;;; is this needed anymore?
;; (with-compilation-unit (:override nil)
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
        (parse-spec (car vec-spec) (cdr vec-spec) 0) )) ;;)

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

(defmacro do-file-by-lines ((line filename) &body body)
  (let ((str (gensym "DO-FILE-BY-LINES-")))
    `(with-open-file (,str ,filename :direction :input)
       (do ((,line (read-line ,str nil) (read-line ,str nil))) ((not ,line) nil)
         ,@body ))))

(defmacro do-file-by (fn (value file) &body body)
  (with-gensyms (str eof "DO-FILE-BY-")
    (let ((loop `(do ((,value #1=(funcall ,fn ,str nil ',eof) #1#))
                     ((eql ,value ',eof) nil)
                   ,@body )))
    `(if (typep ,file 'pathname)
         (with-open-file (,str ,file :direction :input) ,loop)
         (let ((,str ,file)) ,loop) ))))

;(defmacro do-stream-by (fn (value stream ret) &body body)
;  `(do ((,value #1=(funcall ,fn ,stream) #1#))
;       ((eql ,eof ,value) ,ret)
;     ,@body ))

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
(defun head (seq &optional (n 10))
  "Return the first N terms in SEQuence (vector or list).

For vectors, this is a simple wrap around SUBSEQ.  For lists, this
procedure is smarter, namely it does not need to travel the entire
list, just the length you ask for (N)."
  (etypecase seq
    (list (if (or (null seq) (= 0 n))
              nil
              (let ((ret (list (car seq))))
                (funcall (alambda (lst cnt ret end)
                           (cond ((or (null lst) (= 1 cnt)) ret)
                                 (t (setf (cdr end) (list (car lst)))
                                    (self (cdr lst) (1- cnt) ret (cdr end)) )))
                         (cdr seq) n ret ret ))))
    (vector (subseq seq 0 (min n (length seq)))) ))

(defun unroll-circular-list (circular-list n)
  "Create a proper list out of CIRCULAR-LIST that has the same
elements repeating to make a list of length N."
  (cond ((= n 1) (list (car circular-list)))
        (t (cons (car circular-list)
                 (unroll-circular-list (cdr circular-list) (1- n)) ))))

(defun roll-list (list)
  (let ((circular-list (copy-list list)))
    (setf (cdr (last circular-list)) circular-list)
    circular-list ))

;;; tail
(defun tail (seq n)
  (etypecase seq
    (list (list-tail seq n))
    (vector (subseq seq (max (- (length seq) n) 0))) ))
(defvar *empty-sym*
  (gensym "EMPTY-")
  "This symbol is used to mark unused elements.  I need a name that
  the user cannot use, hence the gensym." )
(defun list-tail (lst n)
  "Return the last N elements of LiST.  If (< N (LENGTH LiST)) then
LiST is returned.

This is an iterative function that does not create a new list or cons
anything, really.  As far as lists go, this is about as good as it
gets."
  (let ((start (nthcdr n lst)))
    (if (null start)
        lst
        (do ((lst start (cdr lst))
             (tail lst (cdr tail)) )
            ((and (null lst) (not (eql *empty-sym* (car tail))))
             tail )))))

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

(defmacro /. (args &rest body)
  "A little lambda replacement, the ``/.'' is stolen from the Qi
programming language.  Originally just to save typing and horizontal
space.  Extened it to allow for ignored arguments which are designated
by the ``_'' symbol."
  (let ((arglist (mapcar (lambda (arg) (if (and (symbolp arg)
                                           (equalp (symbol-name arg) "_") )
                                      (cons :gensym (gensym "IGNORED"))
                                      arg ))
                         args )))
    `(lambda ,(mapcar (lambda (arg)
                   (if (and (consp arg)
                            (eql (car arg) :gensym) )
                       (cdr arg)
                       arg )) arglist)
       (declare (ignore ,@(mapcar #'cdr (remove-if-not (lambda (arg)
                                                         (and (consp arg)
                                                              (eql (car arg) :gensym) ))
                                                       arglist ))))
       ,@body )))

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

(defun shadowing-use-package (from-package &optional exclude (to-package *package*))
  "Like USE-PACKAGE except do not import the external symbols listed
in SHADOWS and automatically shadow existing symbols in TO-PACKAGE
that cause name conflicts."
  (let ((syms (remove-if (rcurry #'member exclude) (get-external-symbols from-package))))
    (mapcar (rcurry #'shadowing-import to-package) syms)
    syms ))

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

;; String operations

(defun strcat (&rest args)
  (declare (inline strcat))
  (apply #'concatenate 'string args) )


;; File system operations
(defun copy-directory (from-dir to-dir &key reckless (defaults *default-pathname-defaults*))
  "Copy the entire directory tree from FROM-DIR to TO-DIR.  This is an
attempt to mimic the unix `cp -r' command, which isn't quite as
straight forward as it seems.  The following describes what this does
in terms of the directories as files view of UNIX.

1. Both FROM-DIR and TO-DIR are merged with
*DEFAULT-PATHNAME-DEFAULTS* or keyword value DEFAULTS.

2. If FROM-DIR is a path (ending in a `/'), then remove the `/' and
copy the directory.

3. If TO-DIR is a path, copy FROM-DIR to a subdirectory of TO-DIR from
the most specific directory of FROM-DIR.

4. If TO-DIR is a file, copy the contents of FROM-DIR to TO-DIR
effectively renaming the directory in the move.

5. If there is any danger of overwriting files, continuable errors are
raised unless reckless is true.

Examples:

;; FILE SYSTEM: +-+dir1
;;                +-+sub1
;;                +--file1

;; (copy-directory #p\"dir1\" #p\"dir2\")

;; FILE SYSTEM: +-+dir1
;;                +-+sub1
;;                +--file1
;;              +-+dir2
;;                +-+sub1
;;                +--file1

;; (copy-directory #p\"dir1/\" #p\"dir2/\")

;; FILE SYSTEM: +-+dir1
;;                +-+sub1
;;                +--file1
;;              +-+dir2
;;                +-+dir1
;;                  +-+sub1
;;                  +--file1
;;                +-+sub1
;;                +--file1

TODO/BUGS: 1. We do not resolve symbolic links (due to potability).
           2. File permissions are not copied (again, portability)."
  (let* ((overwrite-dir? (and (not (fad:directory-pathname-p to-dir))
                              (fad:directory-exists-p to-dir) ))
         (dir-exists? (and (fad:directory-pathname-p to-dir)
                           (fad:directory-exists-p
                            (merge-pathnames
                             (make-pathname
                              :name (last1 (pathname-directory from-dir))) to-dir))))
         (from-dir (merge-pathnames from-dir defaults))
         (to-dir (merge-pathnames to-dir defaults)) )
    (cond ((and (not reckless) overwrite-dir?)
           (cerror
            "Go ahead and do it."
            "~a exists, this could trash it. Call with RECKLESS as T to not heed this warning."
            to-dir ))
          ((and (not reckless) dir-exists?)
           (cerror
            "Go ahead and do it."
            "~a exists, this could trash it. Call with RECKLESS as T to not heed this warning."
            (merge-pathnames
             (make-pathname :name (last1 (pathname-directory from-dir))) to-dir )))
          (t 
           (let ((from-dir (if overwrite-dir?
                               (fad:pathname-as-directory from-dir)
                               (fad:pathname-as-file from-dir) ))
                 (to-dir (fad:pathname-as-directory to-dir)) )
             (fad:walk-directory
              from-dir
              (/. (x)
                (let ((to-pathspec
                       (merge-pathnames (enough-namestring x from-dir) to-dir) ))
                  (ensure-directories-exist to-pathspec)
                  (ignore-errors (fad:copy-file x to-pathspec :overwrite t)) ))
              :directories nil ))))))

(defun chop-array (vec length skip &optional (start 0) (end (length vec)))
  "Chop up VECtor from START to END in vectors of length LENGTH,
skiping SKIP elements inbetween segments and return it in a list.  All
vectors returned are of length LENGTH, if the last vector would extend
past END, it is not included.

Ex:
 (chop-array \"hello how are you?\" 2 2 1)
 ==> (\"el\" \" h\"  \" a\"  \" y\")"
  (if (> (+ start length) end) nil
      (cons (fsubvec vec start (min end (+ start length)))
            (chop-array vec length skip (+ start length skip) end) )))

(defun outer-truncate (x)
  (nif x
      (1+ (truncate x))
      0
      (1- (truncate x)) ))

(defun combine-pathnames (pn1 pn2)
  (pathname (strcat (namestring pn1) (namestring pn2))) )

(defun transpose-lists (list-struct)
  (let ((out (make-array (length (car list-struct)) :initial-element nil)))
    (iter (for row in list-struct)
          (iter (for el in row)
                (for i from 0)
                (push el (aref out i)) ))
    (map 'list #'nreverse out) ))

;; Could be dangerous.  I am defining a read macro here that was
;; originally written by Yury Sulsky

;;add python-style multi-line strings
(eval-when (:execute :load-toplevel :compile-toplevel)

  (let ((normal-string-reader (get-macro-character #\")))
    (declare (type function normal-string-reader))
    (defun read-multiline-string (stream c)
      (let ((buffer ()))
        (when (not (char= #\" (peek-char nil stream)))
          (return-from read-multiline-string
            (funcall normal-string-reader stream c)))
        (read-char stream)

        (when (not (char= #\" (peek-char nil stream)))
          (return-from read-multiline-string
            ""))
        (read-char stream)

        (do ((chars (list (read-char stream)
                          (read-char stream)
                          (read-char stream))
                    (cdr (nconc chars (list (read-char stream))))))
            ((every #'(lambda (c) (eq c #\")) chars)
             (coerce (nreverse buffer) 'string))
          (push (car chars) buffer)))))

  (set-macro-character #\" #'read-multiline-string))

(defun rgb<-wavelength (wl &key integer-max (gamma 0.80))
  "Translate a wavelength of light into a rough approximation of what
RGB triple it would corresponds too.  GAMMA adjusts the gamma, and
INTEGER-MAX allows you to specify a scaling factor (like 255) that the
triple will be rounded to.  If WL is outside the visible range, which
we are taking to be 380 - 780nm, we return black.

Apparently doing this right is a very tricky problem, so we find
ourselves with this hack.  This is stolen from Dan Bruton."
  (let ((rgb (cond ((<= 380 wl 440)
                    (list (- (/ (- wl 440) (- 440 380))) 0 1) )
                   ((<= 440 wl 490)
                    (list 0 (/ (- wl 440) (- 490 440)) 1) )
                   ((<= 490 wl 510)
                    (list 0 1 (- (/ (- wl 510) (- 510 490)))) )
                   ((<= 510 wl 580)
                    (list (/ (- wl 510) (- 580 510)) 1 0) )
                   ((<= 580 wl 645)
                    (list 1 (- (/ (- wl 645) (- 645 580))) 0) )
                   ((<= 645 wl 780)
                    (list 1 0 0) )
                   (t (list 0 0 0)) )))
    (flet ((adjust (color factor)
             (if integer-max
                 (round (* integer-max (expt (* color factor) gamma)))
                 (expt (* color factor) gamma) )))
      (cond ((<= 380 wl 420)
             (mapcar (/. (x) (adjust x (+ 0.3 (/ (* 0.7 (- wl 380)) (- 420 380))))) rgb) )
            ((<= 700 wl 780)
             (mapcar (/. (x) (adjust x (+ 0.3 (/ (* 0.7 (- 780 wl)) (- 780 700))))) rgb) )
            (t ;; Either in the middle or out of the visible range
             (mapcar (/. (x) (adjust x 1)) rgb) )))))

(defun maptree (function tree)
  "This calls FUNCTION on each element of TREE that is an atom.  If
you want FUNCTION to operate on lists or any type of cons for that
matter, this function is not for you."
  (mapcar (/. (x) (if (consp x)
                     (maptree function x)
                     (funcall function x) ))
          tree ))

(defun format-ext (str control-string &rest args)
  "Just like format, except convert certain elements in the arg list
into forms more readable by other programs.  For instance, print all
number types in the 1e0 format \(i.e. no fractions or 1d0s), and print
pathnames as namestrings.

Format has all sorts of nooks and crannies, so I bet that this
facility can be broken without too much effort."
  (let ((*read-default-float-format* 'long-float))
    (apply #'format str control-string
           (funcall
            (ttrav #'cons (/. (x) (typecase x
                                   (number (float x 0L0))
                                   (pathname (namestring x))
                                   (t x) )))
            args ))))
;;                     (maptree (/. (x)
;;                                (typecase x
;;                                  (float (float x 0L0))
;;                                  (pathname (namestring x))
;;                                  (t x) ))
;;                              args ))))

(defmacro lambda-in-dyn-env (specials vars &body body)
  "Define an anonymous function with lambda list VARS and BODY which executes
with the specified dynamic variables in SPECIALS bound as they are when it is
declared (not as they are when it is called).  This kind of defeating the
purpose of dynamic variables, but I find it useful when writing callback
functions (which are invoked in code that I did not write and thus have little
control over the dynamic bindings there).  Note that declarations in the body
are handled correctly.

Ex:
 (let ((*print-pretty* nil))
   (lambda-in-dyn-env (*print-pretty*) (some-form some-other-variable)
     (declare (ignore some-other-variable))
     (print some-form) ))

...will print without pretty printing when invoked (no matter that the dynamic
bindings are at that time).

I find it preferable to actually SETFing the dynamic variables.  If anyone has
a better way to do what I am trying to do, I would like to know it (like not
using dynamic variables at all?)."
  (let ((var-names (get-gensyms (length specials))))
    (multiple-value-bind (body-decl body)
        (split-if (/. (x) (not (eql (car x) 'cl:declare))) body)
      `(let ,(group (shuffle var-names specials) 2)
         (lambda ,vars
           ,@body-decl
           (let ,(group (shuffle specials var-names) 2)
             (declare (special ,@specials))
             ,@body ))))))

(defmacro flet-in-dyn-env (specials flets &body body)
  (let ((var-names (get-gensyms (length specials))))
    `(let ,(group (shuffle var-names specials) 2)
       (flet ,(iter (for (name args &body body) in flets)
                    (collecting
                     (list name args
                           `(let ,(group (shuffle specials var-names) 2)
                              (declare (special ,@specials))
                              ,@body ))))
         ,@body ))))

;; Doesn't work.  Shouldn't it?  It would be nicer than making
;; lambda-in-dyn-env, flet-in-dyn-env, etc...

;; (defmacro with-dynamic-environment ((&rest specials) &body body)
;;   (if (null specials)
;;       `(progn ,@body)
;;       (with-gensyms (spec-sym "WITH-DYNAMIC-ENVIRONMENT")
;;         `(let ((,spec-sym ,(first specials)))
;;            (let ((,(first specials) ,spec-sym))
;;              (with-dynamic-environment ,(cdr specials) ,@body) ))) ))

(defmacro dbp (&rest forms)
  "DeBug Pring: A little macro that prints several forms.  Mainly this
is to make removing debugging print statement simpler since, unlike
PRINT, DBP is only used for debugging prints.  In the future I might
make a conditional macroexpand that will only print if certain debug
flags are set, maybe."
  `(progn
     (format *error-output*
             "~%DBP:~{~%~{~S ~^= ~}~}"
             (mapcar (/. (x y) (list x y)) ',forms (list ,@forms) ))))

(defun nd-index (linear extents)
  "Given a row major linear index and a list of array extents
\(dimensions) return a list of N-D array indicies."
  (iter (for ext on (append (cdr extents) (list 1)))
        (let* ((slab-size (apply #'* ext))
               (idx (floor linear slab-size)) )
          (decf linear (* slab-size idx))
          (collect idx) )))

(defmacro splice-@ (fn &rest args)
  "Acts sort of like a mix of APPLY and the ,@ operator.
Splice the @ marked lists into the sexp.  This is done by building a
list and applying the function to it.  Because the function is applied
to the arglist, you have to pass a function descriptor, not function
name.

\(splice-@ #'+ 1 2 @(list 3 4) 5 6) => 21"
  (with-gensyms (new-args)
    (let ((plain-args (iter (for form in args)
                            (until (and (symbolp form)
                                        (equalp (symbol-name form) "@") ))
                            (collect form) )))
      `(let* ((,new-args
               (append
                ,@(let (splice spliced?)
                    (iter (for form in args)
                          (cond (splice (tb:toggle splice) (collect form into final))
                                ((and (symbolp form)
                                      (equalp (symbol-name form) "@") )
                                 (setf spliced? t)
                                 (tb:toggle splice)
                                 (when tmp
                                   (collect (cons 'list tmp) into final)
                                   (setf tmp nil) ))
                                (spliced? (collect form into tmp)) )
                          (finally
                           (return (if tmp
                                       (nconc final (list (cons 'list tmp)))
                                       final ))))))))
         (apply ,fn ,@plain-args ,new-args) ))))


(defun copy-instance (instance)
  "Make a copy of an instance of ony class."
  (let* ((class (class-of instance))
         (slots (closer-mop:class-slots class))
         (new-instance (make-instance class)))
    (loop for slot in slots do
      (setf (slot-value new-instance (closer-mop:slot-definition-name slot))
            (slot-value instance (closer-mop:slot-definition-name slot))))
    new-instance))

(defun expand-obj-fn (fn new-obj new-car slot &rest obj-or-more-conses)
  (if (= 1 (length obj-or-more-conses))
      (setf (slot-value new-obj slot)
            (funcall fn new-car (slot-value (last1 obj-or-more-conses) slot)) )
      (progn
        (setf (slot-value new-obj slot)
              (funcall fn new-car (slot-value (last1 obj-or-more-conses) slot)) )
        (apply #'expand-obj-fn fn new-obj obj-or-more-conses) )))

(defun obj-fn (fn arg slot &rest obj-or-more-conses)
  "Create a new object where each slot listed is set equal to \(FN ARG
\(SLOT-VALUE OBJ SLOT))."
  (let ((new-obj (copy-instance (last1 obj-or-more-conses))))
    (apply #'expand-obj-fn fn new-obj arg slot obj-or-more-conses)
    new-obj ))

(defun obj-cons (new-car slot &rest obj-or-more-conses)
  "Return new object where specified SLOTs are modified by consing on
the NEW-CAR."
  (apply #'obj-fn #'cons new-car slot obj-or-more-conses) )

(defun mp (&rest pathspecs)
  "Merge pathnames and namestrings in a logical way."
  ;;(cond ((
  (cond ((null pathspecs)
         *default-pathname-defaults* )
        (t (merge-pathnames (first pathspecs)
                            (apply #'mp (rest pathspecs)) ))))

(defun find-fbound-symbols (package-name)
  (let ((result nil)
        (p (find-package package-name)))
    (do-symbols (s p result)
      (when (and (equal (symbol-package s) p)
                 (fboundp s))
        (push s result)))))

(defun trace-package (package)
  (iter (for sym in (find-fbound-symbols package))
        (ignore-errors (eval `(trace ,sym))) )
  (format nil "Package ~A is now traced." package))

(defun untrace-package (package)
  (eval `(untrace ,@(find-fbound-symbols package)))
  (format nil "Package ~A is now untraced." package))

(defun untrace-all ()
  (untrace))

(defun char-upcase-p (char)
  (eql (char-upcase char) char) )
(defun char-downcase-p (char)
  (eql (char-downcase char) char) )

(defun invert-case (string &key hyphen-to-underscore)
  (let ((new-string (make-string (length string))))
    (iter (for char in-sequence string)
          (for i from 0)
          (setf (aref new-string i)
                (cond ((and hyphen-to-underscore (eql char #\-))
                       #\_ )
                      ((and hyphen-to-underscore (eql char #\_))
                       #\- )
                      ((char-upcase-p char)
                       (char-downcase char) )
                      (t (char-upcase char)) )))
    new-string ))
