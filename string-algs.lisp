
(in-package :toolbox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; String difference/comparison algorithms ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Levenshtein Distance

;;; Efficient implementation via memoization (a.k.a. dynamic programming) O(n*m)
(labels ((safe-elt (seq index)
           (elt seq (1- index)) )
         (r (a b)
           (if (eql a b) 0 1) ))
  (declare (notinline levenshtein-dist)) ; For memoization to work
  (defun levenshtein-dist (s1 s2 &optional (i (length s1)) (j (length s2)) (wi 1) (wd 1) (wo 1))
    (cond ((= j 0) i)
          ((= i 0) j)
          (t (min (1+ (levenshtein-dist s1 s2 (1- i) j))
                  (1+ (levenshtein-dist s1 s2 i (1- j)))
                  (+ (levenshtein-dist s1 s2 (1- i) (1- j))
                     (r (safe-elt s1 i) (safe-elt s2 j)) ))))))
(setf (symbol-function 'levenshtein-dist) (memoize #'levenshtein-dist :test #'equalp))

;; Examples

;; (levenshtein-dist "Su" "Sa")

;; (levenshtein-dist "saturday" "sunday")

;; (levenshtein-dist "kitten" "sitting")

;; (levenshtein-dist "funky" "monkey")



;;; Longest Common Subsequence

;;; Naive inefficient implementation...
(defun len-max (s1 s2)
  (if (< (length s1) (length s2)) s2 s1) )

(defun lcs (s1 s2 &key (base ""))
  (declare (notinline lcs)) ; This needs to be noninlined for memoization to work
  (cond ((or (= (length s1) 0) (= (length s2) 0)) base)
        ((eql (elt s1 0) (elt s2 0))
         (concatenate 'vector (subseq s1 0 1) (lcs (subseq s1 1) (subseq s2 1) :base base)) )
        (t (len-max (lcs s1 (subseq s2 1) :base base) (lcs (subseq s1 1) s2 :base base))) ))

;;; ...made into an efficient implementation via one line of code
(setf (symbol-function 'lcs) (memoize #'lcs :test #'equalp))

;; Examples

;; (time (lcs "xaxbxcxdef" "abcdexfxgxhi"))

;;; This example needs the more relaxed equalp test or it will incurr all of the
;;; hash table overhead and none of the dynamic programming benefits, making it
;;; really crawl
;; (time (lcs #(1 2 5 2 4 3 2 256 255 254) #(7 6 3 4 1 2 256 255 254) :base #()))

;;; Do not try without memoization
;; (time (lcs "asdfklj asdflkj dlaskd jlksd alkdj lakdsfj lsdkjdkjd flksjd"
;;            "lkas djflkasjdflkasdj fsd skldjfkdj dkljdksdj fkldkdkdj slkdj f") )


;;; PPCRE extensions
(defun reg-scan (regex target-string &key (start 0) (end (length target-string)))
  (multiple-value-bind (start end r-start r-end) (ppcre:scan regex target-string :start start :end end)
    (if (< 0 (length r-start))
        (values (aref r-start 0) (aref r-end 0))
        nil )))

;; What I use these for can often be obtained via register-groups-bind, but I
;; cannot use it for exactly that purpose, so these remain.

(defun reg-scan-to-string (regex target-string &key (start 0) (end (length target-string)))
  (ppcre:register-groups-bind (a) (regex target-string :start start :end end) a))
  ;; (multiple-value-bind (matches registers) (ppcre:scan-to-strings regex target-string :start start :end end)
  ;;   (declare (ignore matches))
  ;;   (values-list (iter (for el in-sequence registers)
  ;;                      (collect el) ))))

(defun reg-scan-to-strings (regex target-string
                                  &key (start 0) (end (length target-string)) )
  (multiple-value-bind (matches registers)
      (ppcre:scan-to-strings regex target-string :start start :end end)
    (declare (ignore matches))
    (coerce registers 'list) ))

