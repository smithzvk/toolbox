
(asdf:defsystem #:toolbox
  :name "toolbox"
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "GPL"
  :description "Some functions and macros I have accumulated"
  :components ((:file "package")
               (:file "on")
               (:file "anaphoric")
               (:file "iterate")
               (:file "dbind")
               (:file "applied")
               (:file "mvb")
               (:file "fcase")
               (:file "misc")
               (:file "compat")
;               (:file "b-io")
               (:file "numerics")
               (:file "string-algs")
               (:file "dynamic-programming")
               (:file "number-theory"))
  :serial t
  :depends-on (:anaphora :alexandria :cl-fad :iterate :cl-ppcre
                         :pythonic-string-reader
                         :cl-primality
                         :cl-factoring
                         :bordeaux-threads
                         #-abcl :closer-mop ))

