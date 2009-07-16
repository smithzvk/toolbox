(in-package :toolbox)

(defmacro uflow->zero (&body body)
  "Map a gradual underflow onto the floating point value zero.  This
could probably be done more cleanly and reliably by handling the
conditions"
  #+clisp `(ext:without-floating-point-underflow ,@body)
  #+ecl `(progn (si:trap-fpe 'floating-point-underflow nil)
                (prog1 (progn ,@body)
                  (si:trap-fpe 'floating-point-underflow t) ))
  #-(or ecl clisp) `(progn ,@body) )

(defun =~ (tol &rest args)
  "Test if the ARGS are all within TOLerance of one another"
  (< (abs (- (apply #'max args) (apply #'min args))) tol) )

#| Examples

\(=~ 1d-2 1.414 (sqrt 2) (sqrt 2.001))

\(apply (cute #'=~ 1d-2) (list 1.414 (sqrt 2)))

|#
