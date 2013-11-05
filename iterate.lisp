
(in-package :toolbox)

(iter:defmacro-clause (average expr
                               &optional
                               by (addition-op '#'+)
                               using-divisor (division-op '#'/)
                               initial-value (initial-value 0) )
  (let ((total (gensym))
        (count (gensym)) )
    `(progn
       (with ,total = ,initial-value)
       (with ,count = 0)
       (setf ,total (funcall ,addition-op ,total ,expr))
       (incf ,count)
       (finally (return (funcall ,division-op ,total ,count))) )))

(iter:defsynonym averaging average)

(defmacro on-every-nth-iteration (n &body body)
  (let ((index (gensym)))
    `(progn (for ,index from 0)
            (when (= ,index ,n)
              (setf ,index 0)
              ,@body ))))

(defmacro with-return-work-so-far-restart (&body body)
  "This is useful for iterate as it will allow you to get partial results
\(e.g. if your loop hangs or is simply taking too long this will allow you to
get what has been done so far).

Use this something like this:

  (iter (for i :below 100)
    (tb:with-return-work-so-far-restart
      (sleep 1)
      (collecting i)))
"
  `(restart-case
       (progn ,@body)
     (return-work-so-far ()
       (finish))))
