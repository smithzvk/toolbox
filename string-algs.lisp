
(in-package :toolbox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; String difference/comparison algorithms ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Levenshtein Distance

#| Horribly inefficient  straight-forward implementation (exp)
(defun levenshtein-dist (s1 s2)
  (labels
    ((d (i j)
       (cond ((= j 0) i)
             ((= i 0) j)
             (t (min (1+ (d (1- i) j)) ; deletion
                     (1+ (d i (1- j))) ; insertion
                     (+ (d (1- i) (1- j)) ;substitution
                        (r (s-elt s1 i) (s-elt s2 j)) )))))
     (s-elt (seq index)
       (elt seq (1- index)) )
     (r (a b)
       (if (eql a b) 0 1) ))
    (d (length s1) (length s2)) ))

|#

;;; Efficient implementation via memoization (a.k.a. dynamic programming) O(n*m)
(defun levenshtein-dist (s1 s2)
  (labels
    ((d (rec i j)
       (format t "~a ~a~%" i j)
       (cond ((= j 0) i)
             ((= i 0) j)
             (t (min (1+ (funcall rec rec (1- i) j)) ; deletion
                     (1+ (funcall rec rec i (1- j))) ; insertion
                     (+ (funcall rec rec (1- i) (1- j)) ;substitution
                        (r (s-elt s1 i) (s-elt s2 j)) )))))
     (s-elt (seq index)
       (elt seq (1- index)) )
     (r (a b)
       (if (eql a b) 0 1) ))
    (let ((d-mem (memoize #'d)))
      (funcall d-mem d-mem (length s1) (length s2)) )))

#| Examples
(levenshtein-dist "Saturday" "Sunday")

(levenshtein-dist "kitten" "sitting")

|#

;;; Longest Common Subsequence

;;; Naive inefficient implementation...
(defun len-max (s1 s2)
  (if (< (length s1) (length s2)) s2 s1) )

(defun lcs (s1 s2 &key (base ""))
  (cond ((or (= (length s1) 0) (= (length s2) 0)) base)
        ((eql (elt s1 0) (elt s2 0))
         (concatenate 'vector (subseq s1 0 1) (lcs (subseq s1 1) (subseq s2 1) :base base)) )
        (t (len-max (lcs s1 (subseq s2 1) :base base) (lcs (subseq s1 1) s2 :base base))) ))

;;; ...made into an efficient implementation via one line of code
(setf (symbol-function 'lcs) (memoize #'lcs :test #'equalp))

#| Examples

;;; Changing to (or (< debug speed) (= 0 debug)) causes the memoization to suck
;;; not sure why...
;;; (declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))
(time (lcs "xaxbxcxdef" "abcdexfxgxhi"))

;;; This example needs the more relaxed equalp test or it will incurr all of the
;;; hash table overhead and none of the dynamic programming benefits, making it
;;; really crawl
(time (lcs #(1 2 5 2 4 3 2 256 255 254) #(7 6 3 4 1 2 256 255 254) :base #()))

;;; Do not try without memoization
(time (lcs "asdfklj asdflkj dlaskd jlksd alkdj lakdsfj lsdkjdkjd flksjd"
           "lkas djflkasjdflkasdj fsd skldjfkdj dkljdksdj fkldkdkdj slkdj f") )

|#

