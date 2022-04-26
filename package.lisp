(defpackage #:sbcl-script
  (:use :cl
        :cl-getopt
        :dlist)
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
   :safe-string
   :safe-read-from-string
   :wrap-string
   ;; files
   :mktemp
   :with-temp
   :tmppath
   :make-directory-pathname
   :subpath
   :homepath
   :read-entire-stream
   ;; running commands
   :run
   :start
   :enable-run-reader-macro
   ;; error handling
   :run-error
   :ignore-error
   ))
