
(in-package :toolbox)

;; None are mine, but very handy.

;; Read environment variables
(defun getenv (name &optional default)
  "Get an environment variable."
  #+cmu
  (let (( x (assoc name ext:*environment-list*
                   :test #'string= )))
    (if x (cdr x) default) )
  #-cmu
  (or #+allegro (sys:getenv name)
      #+clisp (ext:getenv name)
      #+ecl (si:getenv name)
      #+sbcl (sb-unix::posix-getenv name)
      #+lispworks (lispworks:environment-variable name)
      #-(or allegro clisp ecl sbcl lispworks)
      (progn (warn "Don't know how to get environment variables in this Lisp.  Using the default."))
      default ))

;; Read command line arguements
(defun command-line ()
  "Get `groomed' command line arguments.  These should just be the
arguments left to you program."

  #+sbcl (cdr sb-ext:*posix-argv*)
  #+lispworks system:*line-arguments*
  #+cmu extensions:*command-line-words*
  #+clisp ext:*args*
  #-(or sbcl lispworks cmu clisp)
  (error "Don't know how to get command line arguments in this Lisp.") )

(defun raw-command-line ()
  "Get the raw command line.  Not very useful as it may contain all
sorts of implementation dependent stuff.  See COMMAND-LINE for
something a bit more useful."

  #+sbcl sb-ext:*posix-argv*
  #+clisp (ext:argv)
  ;; I don't have these imps
  #+(or lispworks allegro)
  (command-line)
  ;; Seems that command-line is the best I can do
  #+(or cmu)
  (command-line)
  #-(or sbcl clisp lispworks allegro cmu)
  (error "Don't know how to get command line arguments in this Lisp.") )


;; From CLiki
(defun quit (&optional code)
      ;; This group from "clocc-port/ext.lisp"
      #+allegro (excl:exit code)
      #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
      #+cmu (ext:quit code)
      #+cormanlisp (win32:exitprocess code)
      #+gcl (lisp:bye code)                     ; XXX Or is it LISP::QUIT?
      #+lispworks (lw:quit :status code)
      #+lucid (lcl:quit code)
      #+sbcl (sb-ext:quit
              :unix-code (typecase code (number code) (null 0) (t 1)))
      ;; This group from Maxima
      #+kcl (lisp::bye)                         ; XXX Does this take an arg?
      #+scl (ext:quit code)                     ; XXX Pretty sure this *does*.
      #+(or openmcl mcl) (ccl::quit)
      #+abcl (cl-user::quit)
      #+ecl (si:quit)
      ;; This group from 
      #+poplog (poplog::bye)                    ; XXX Does this take an arg?
      #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
            kcl scl openmcl mcl abcl ecl)
      (error 'not-implemented :proc (list 'quit code))) 
;; (defun quit (&optional (exit-code 0))
;;   #+openmcl (ccl:quit exit-code)
;;   #+sbcl (sb-ext:quit :unix-status exit-code)
;;   #+clisp (ext:quit exit-code)
;;   #+(or cmu allegro) (declare (ignore exit-code))
;;   #+cmu (ext:quit)
;;   #+lispworks (lispworks:quit :status exit-code)
;;   #+allegro (excl:exit)
;;   #-(or openmcl sbcl cmu allegro lispworks)
;;   (error "Don't know how to quit this Lisp.") )


;;; Multiprocessing

(defmacro without-interrupts (&body body)
  #+sbcl `(sb-sys:without-interrupts ,@body)
  #+ecl `(mp:without-interrupts ,@body)
  #+cmucl `(system:without-interrupts ,@body)
  #+ccl `(ccl:without-interrupts ,@body)
  #-(or sbcl ecl cmucl ccl)
  (progn
    (warn "I don't know how to disable interrupts in this implementation")
    `(progn ,@body) ))
