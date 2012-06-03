(in-package :toolbox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Modular Arithmetic ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun *-mod (n m md)
  (mod (* n m) md) )

(defun expt-mod (b e md &optional (tot 1))
  (declare (type integer e))
  (cond ((= e 0) tot)
        ((oddp e)
         (expt-mod (mod (* b b) md)
                   (ash e -1)
                   md
                   (mod (* tot b) md)) )
        (t (expt-mod (mod (* b b) md)
                     (ash e -1)
                     md
                     tot ))))

;;; something like this would be nice
;; (defmacro with-modulo-ops (modulus &body body)
;;   (cond ((and (listp body) (atom (car body)))
;;          (cond ((eql '+ (car body)) `(mod ,(with-modulo-ops ) ,modulus)
;;                                     )))))

;; (with-modulo-ops m
;;   (+ 5 (* 342 (expt 2 500))) )
;;   ==> (let ((m m))
;;         (mod (+ 5 (mod (* 342 (expt-mod 2 500 m)) m)) m) )


