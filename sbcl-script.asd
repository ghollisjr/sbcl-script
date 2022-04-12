(asdf:defsystem #:sbcl-script
    :author "Gary Hollis"
    :description "A library for writing scripts with Steele Bank Common Lisp"
    :license "GPLv3"
    :depends-on (#:cl-getopt
                 #:cl-fad
                 #:str
                 #:dlist
                 #:uiop)
    :components
    ((:file "package")
     (:file "strings")
     (:file "env")
     (:file "files")
     (:file "run")))
