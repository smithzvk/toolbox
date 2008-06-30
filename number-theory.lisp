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

#|
;;; something like this would be nice
(defmacro with-modulo-ops (modulus &body body)
  (cond ((and (listp body) (atom (car body)))
         (cond ((eql '+ (car body)) `(mod ,(with-modulo-ops ) ,modulus)
                                    )))))

(with-modulo-ops m
  (+ 5 (* 342 (expt 2 500))) )
  ==> (let ((m m))
        (mod (+ 5 (mod (* 342 (expt-mod 2 500 m)) m)) m) ) 
|#

;;;;;;;;;;;;;;;;;;;
;;;; Primality ;;;;
;;;;;;;;;;;;;;;;;;;

;;; Miller-Rabin algorithm

(with-compilation-unit (:override nil)
  (defun miller-rabin (n &optional (chance-of-error 1d-10))
    "MILLER-RABIN probabilistic primality test:
    Checks if N is prime with the chance of a false positive less than
    CHANCE-OF-ERROR.  This algorithm never gives false negatives."
    (declare (optimize (speed 3) (debug 0)))
    (cond ((= n 1) nil)
          ((= n 2) n)
          (t (let ((n-iter (ceiling (log chance-of-error 1/4))))
               (funcall (alambda (n n-iter)
                          (cond ((= n-iter 0) n)
                                (t (and (miller-rabin-pass n (1+ (random (1- n))))
                                        (self n (1- n-iter)) ))))
                        n n-iter )))))
  (defun miller-rabin-pass (n a)
    (declare (optimize (speed 3) (debug 0))
             (inline miller-rabin-pass) )
    (labels ((decompose-val (n s)
               (cond ((or (= n 0) (oddp n)) (values n s))
                     (t (decompose-val (/ n 2) (1+ s))) )))
      (mvb (d s) (decompose-val (1- n) 0)
        (cond ((= 1 (expt-mod a d n)) n)
              ((do* ((a-loc (expt-mod a d n) (expt-mod a-loc 2 n))
                     (i 0 (1+ i))
                     (ret (= (1- n) a-loc) (= (1- n) a-loc)) )
                    ((or ret (= i s)) (if (/= i s) t)) ) n )
              (t nil) ))))
  (defun gen-prime (n-bits &optional (prime? #'miller-rabin))
    "Generate a prime that is N-BITS long (less than 2^N-BITS).  Just
try random number of the right length until we find one that is
prime (we use MILLER-RABIN for the test here)."
    (declare (optimize (speed 3) (debug 0)))
    (let ((max (1- (expt 2 n-bits))))
      (aif (funcall prime? (1+ (* 2 (random max))))
           it
           (gen-prime n-bits prime?) ))) )

;;; Define some common name interfaces
(defwrapper prime? miller-rabin)

;; Examples

;; We are extremely sure that this is prime
;; (miller-rabin 101 1d-200)

;; (time (gen-prime 128) )
;; (time (gen-prime 256) )
;; (time (gen-prime 512) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; General Factoring ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This doesn't really work
(defun coprime-factor-trial-division (n)
  (cond ((prime? n) n)
        (t (do ((i 2 (1+ i)))
               ((or (integerp (/ n i))
                    (>= i (sqrt n)) )
                (list i (/ n i)) )))))

;;; Trial division O(sqrt(n)/2) about as bad as it can get

(defun factor-trial-division (n)
  (cond ((prime? n) (list n))
        (t (apply #'append
                  (mapcar #'factor-trial-division
                          (do ((i 2 (1+ i)))
                              ((or (integerp (/ n i))
                                   (> i (sqrt n)) )
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dealing with digits ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(with-compilation-unit (:override nil)
  (defun n-digits (n)
    (funcall
      (alambda (n count)
        (cond ((= n 0) count)
              (t (self (floor n 10) (1+ count))) ))
      n 0 ))
  (defun get-digit (n m)
    (cond ((= m 0) (mod n 10))
          (t (get-digit (floor n 10) (1- m))) ))
  (defun digits<-number (n)
    (declare (type integer n))
    (funcall
      (alambda (n m ret)
        (cond ((= m 1) (cons (mod n 10) ret))
              (t (self (floor n 10) (1- m) (cons (mod n 10) ret))) ))
      n (n-digits n) nil ))
  (defun number<-digits (digits)
    (declare (type list digits))
    (funcall
      (alambda (digits pow tot)
        (cond ((null digits) tot)
              (t (self (cdr digits) (1+ pow)
                       (+ tot (* (car digits) (expt 10 pow))) ))))
      (reverse digits) 0 0) ) )

#| Examples

(n-digits 321)
(n-digits (floor 1d100))
(n-digits (expt 2 32)) ; 2^32 ~= 4e9

(mapcar #'(lambda (n) (get-digit 12345 n)) '(0 1 2 3 4))

(digits<-number 12345)
(number<-digits (digits<-number 54321))

|#

