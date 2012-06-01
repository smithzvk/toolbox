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


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General Factoring ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This doesn't really work
(defun coprime-factor-trial-division (n)
  (cond ((prime? n) n)
        (t (do ((i 2 (1+ i)))
               ((or (integerp (/ n i))
                    (>= i (isqrt n)) )
                (list i (/ n i)) )))))

;;; Trial division O(sqrt(n)/2) about as bad as it can get

(defun factor-trial-division (n)
  (cond ((prime? n) (list n))
        (t (apply #'append
                  (mapcar #'factor-trial-division
                          (do ((i 2 (1+ i)))
                              ((or (integerp (/ n i))
                                   (> i (isqrt n)) )
                               (list i (/ n i)) )))))))

;;; Shank's square forms factorization O(n^(1/4))

;;; Dixon's factorization method O(e^(2 sqrt(2) sqrt(log n log log n)))

;;; Continued fraction factorization O(e^sqrt(2 log n log log n))

;;; Quadratic sieve O(e^sqrt(log n log log n))
;;; Fastest known algorithm for numbers under 100 decimal digits

;;; General number field sieve O(e^((c+o(1))(log n)^(1/3) (log log n)^(2/3))) (heuristically)
;;; Assymptotically fastest known algorithm

;;; Define some common name interfaces
(defwrapper factor factor-trial-division)
(defwrapper coprime-factor coprime-factor-trial-division)
