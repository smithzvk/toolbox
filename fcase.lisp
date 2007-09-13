;;;; A simple macro that acts likes case, except it can use arbitrary test 
;;;; functions.  It was stolen from the CLISP source.  For the most part, it
;;;; is a series of nested cond statements playing dress-up.


#-clisp
(defun case-expand (whole-form form-name test keyform clauses)
  (let ((var (gensym (concatenate 'string (symbol-name form-name) "-KEY-"))))
    `(let ((,var ,keyform))
      (cond
        ,@(maplist
           #'(lambda (remaining-clauses)
               (let ((clause (first remaining-clauses))
                     (remaining-clauses (rest remaining-clauses)))
                 (unless (consp clause)
                   (format t "~a: missing key list" whole-form)
                   (break) )
                 (let ((keys (first clause)))
                   `(,(cond ((or (eq keys 'T) (eq keys 'OTHERWISE))
                             (if remaining-clauses
                                 (format t "~a: the ~a clause must be the last one"
                                         whole-form clause )
                                 't))
                            ((listp keys)
                             `(or ,@(mapcar #'(lambda (key)
                                                `(,test ,var ',key))
                                            keys)))
                            (t `(,test ,var ',keys)))
                     ,@(rest clause)))))
           clauses)))))

#-clisp
(defmacro fcase (&whole whole-form
                 test keyform &body clauses)
  (case-expand whole-form 'fcase test keyform clauses))

