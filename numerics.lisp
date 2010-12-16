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

(defun sign (x)
  (cond ((< x 0) -1)
        (t 1) ))

(defun find-root (fn low high &optional (precission 1d-6))
  "Find a root of FN between LOW and HIGH to within PRECISSION."
  (unless (/= (sign (funcall fn low)) (sign (funcall fn high)))
    (error "In order for this to work, (FN LOW) and (FN HIGH) need to be on opposite sides of zero.") )
  (let ((sign (sign (funcall fn low))))
    (labels
        ((%find-root (low high)
           (if (< (- high low) precission) (/ (+ low high) 2)
               (let* ((midway (/ (+ low high) 2))
                      (val (* sign (funcall fn midway))) )
                 (if (> val 0)
                     (%find-root midway high)
                     (%find-root low midway) )))))
      (%find-root low high) )))

