(defpackage #:sbcl-script
  (:use :cl
        :cl-getopt)
  (:import-from :sb-posix
                :chdir
                :getcwd)
  (:export
   :chdir
   :getcwd
   :getopt
   :option-descriptions
   :exe
   :exebg
   :with-interrupt
   :whitespace-p
   ;; files
   :mktemp
   :with-temp
   :tmppath
   :make-directory-pathname
   :subpath
   ;; running commands
   :run
   :start
   :enable-run-reader-macro
   ))
