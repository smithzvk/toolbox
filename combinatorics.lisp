(in-package :toolbox)

(defun ! (n)
  (cond ((or (= n 1) (= n 0)) 1)
        (t (* n (! (1- n)))) ))

(defun permute (n k)
  (/ (! n)
     (! (- n k)) ))

(defun choose (n k)
  (/ (! n)
     (* (! (- n k))
        (! k) )))

