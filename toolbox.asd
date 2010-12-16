
(asdf:defsystem #:toolbox
  :name "toolbox"
  :author "Zachary Smith <elzacho@gmail.com>"
  :license "GPL"
  :description "Some functions and macros I have accumulated"
  :components ((:file "package")
               (:file "on")
               (:file "anaphoric")
               (:file "dbind")
               (:file "applied")
               (:file "mvb")
               (:file "fcase")
               (:file "misc")
               (:file "compat")
;               (:file "b-io")
               (:file "numerics")
               (:file "string-algs")
               (:file "number-theory")
               (:file "infix") )
  :serial t
  :depends-on (:anaphora :alexandria :cl-fad :iterate :cl-ppcre :closer-mop) )

