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
   ;; files
   :mktemp
   :with-temp
   ;; running commands
   :run
   :start
   :enable-run-reader-macro
   ))
