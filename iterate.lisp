
(in-package :toolbox)

(iter:defmacro-clause (average expr)
  (let ((total (gensym))
        (count (gensym)) )
    `(progn
       (with ,total = 0)
       (with ,count = 0)
       (incf ,total ,expr)
       (incf ,count)
       (finally (return (/ ,total ,count))) )))

(iter:defsynonym averaging average)
