(in-package :toolbox)

(defmacro uflow->zero (&body body)
  #+clisp `(ext:without-floating-point-underflow ,@body)
  #-clisp `(progn ,@body) )
  ;#+(or sbcl cmu) `(progn ,@body) )

(defun =~ (tol &rest args)
  (< (abs (- (apply #'max args) (apply #'min args))) tol) )

#| Examples

(=~ 1d-2 1.414 (sqrt 2) (sqrt 2.001))

(apply (cute #'=~ 1d-2) (list 1.414 (sqrt 2)))

|#
