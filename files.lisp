(in-package :sbcl-script)

(defun mktemp (&optional directory-p)
  "Creates temporary file or directory and returns path string"
  (if directory-p
      (concatenate 'string
                   (butnewline
                    (run "mktemp"
                         (list "-d")))
                   "/")
      (butnewline
       (run "mktemp" nil))))

(defmacro with-temp ((path &optional directory-p) &body body)
  "Creates temporary or directory and binds the supplied path symbol
to the value of the path string during the execution of the supplied
code body.  Also when directory-p is non-NIL, a function #'tmppath is
available to the code body that accepts format-style arguments and
returns temporary paths below the generated tmp directory.

E.g.: (let ((x \"test.txt\")) (tmppath \"~a\" x))
=> <tmpdir>/test.txt"
  (let* ((result (gensym "result")))
    `(let* ((,path (mktemp ,directory-p))
            (,result
             ,(if (not directory-p)
                  `(progn ,@body)
                  `(flet ((tmppath (&rest format-args)
                            (let* ((tmpstr (apply #'format nil format-args))
                                   (p (pathname tmpstr)))
                              (namestring
                               (merge-pathnames p
                                                (make-pathname :directory
                                                               (pathname-directory ,path)))))))
                     ,@body))))
       ,(if directory-p
            `(cl-fad:delete-directory-and-files ,path)
            `(delete-file ,path))
       ,result)))
