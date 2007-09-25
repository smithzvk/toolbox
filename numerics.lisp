(in-package :toolbox)

(defmacro uflow->zero (&body body)
  #+clisp `(ext:without-floating-point-underflow ,@body)
  #-clisp `(progn ,@body) )
  ;#+(or sbcl cmu) `(progn ,@body) )

