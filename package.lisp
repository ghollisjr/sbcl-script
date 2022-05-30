(defpackage #:sbcl-script
  (:use :cl
        :cl-getopt
        :dlist)
  (:import-from
   :sb-posix
   :chdir
   :getcwd)
  (:import-from
   :uiop
   :ensure-directory-pathname
   :delete-directory-tree
   :delete-empty-directory
   :delete-file-if-exists)
  (:export
   ;; sb-posix
   :chdir
   :getcwd
   ;; uiop
   :ensure-directory-pathname
   :delete-directory-tree
   :delete-empty-directory
   :delete-file-if-exists
   ;; sbcl-script
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
