(asdf:oos 'asdf:load-op 'ieee-floats)
(use-package :ieee-floats)
(asdf:oos 'asdf:load-op 'packer)
(use-package :packer)

(defun size-of (data-type)
  (case data-type
    ((double-float) 8)
    ((single-float integer) 4)
    ((byte unsigned-byte) 1)
    (otherwise (break)) ))

(defun b-write (str seq data-type &key (endian "<"))
  (multiple-value-bind (encode-func format-string)
    (case data-type
      ((double-float) (values #'encode-float64 "L"))
      ((single-float) (values #'encode-float32 "I"))
      (otherwise (values #'identity "I")) )
    (setf format-string 
          (concatenate 'string endian (mkstr (length seq)) format-string) )
    (write-sequence
      (apply #'pack (cons format-string (mapcar encode-func seq))) str )))


(defun b-read (str num data-type &optional
                   (seq (make-array (* (size-of data-type) num) 
                                    :element-type '(unsigned-byte 8) ))
                   (endian "<") )
  (multiple-value-bind (decode-func format-string)
    (case data-type
      ((double-float) (values #'decode-float64 "L"))
      ((single-float) (values #'decode-float32 "I"))
      (otherwise (values #'identity "I")) )
    (setf format-string 
          (concatenate 'string endian (mkstr num) format-string) )
    (mapcar decode-func
            (unpack format-string (progn (read-sequence seq str) seq)) )))

#|
(with-open-file (b-out #p"b-out"
                       :direction :output
                       :if-exists :supersede
                       :element-type 'unsigned-byte )
  (b-write b-out '(564.5311d0 123.12d0 21.121234d0) 'double-float) )

(with-open-file (b-in #p"spheres.3.final_config"
                       :direction :input
                       :element-type 'unsigned-byte )
  (let* ((n-dim (b-read b-in 1 'integer))
         (n-sites (b-read b-in 1 'integer))
         (h (b-read b-in 9 'double-float))
         (p1 (b-read b-in 3 'double-float))
         (trash (b-read b-in 3 'double-float))
         (p2 (b-read b-in 3 'double-float))
         (trash (b-read b-in 3 'double-float))
         (p3 (b-read b-in 3 'double-float)) )
    (list n-dim n-sites h p1 p2 p3) ))
|#
