
(in-package :toolbox)

(iter:defmacro-clause (average expr
                               &optional
                               by (addition-op  #'+)
                               using-divisor (division-op #'/)
                               initially (initial-value 0) )
  (let ((total (gensym))
        (count (gensym)) )
    `(progn
       (with ,total = ,initial-value)
       (with ,count = 0)
       (setf ,total (funcall ,addition-op ,total ,expr))
       (incf ,count)
       (finally (return (funcall ,division-op ,total ,count))) )))

(iter:defsynonym averaging average)
