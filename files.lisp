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

(defun make-directory-pathname (path)
  "Returns path to directory for given pathname, ensuring that all parts
of the pathname are treated as the directory.  NOTE: This is
deprecated, as uiop:ensure-directory-pathname already provides this
functionality."
  (let* ((p (pathname path))
         (name (pathname-name p))
         (type (pathname-type p))
         (dir (pathname-directory p))
         (basepath (make-pathname :name name
                                  :type type))
         (basename (namestring basepath)))
    (merge-pathnames
     (make-pathname :directory (list :relative basename))
     (make-pathname :directory dir))))

(defun subpath (relpath dirpath)
  "Returns pathname for relative path below dirpath.  Slightly more
convenient than merge-pathnames since it uses
#'uiop:ensure-directory-pathname."
  (let* ((dirpath (ensure-directory-pathname dirpath)))
    (merge-pathnames relpath dirpath)))

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

(defun homepath (&rest format-args)
  "Returns subpath with home directory as the implicit dirpath."
  (namestring
   (subpath (apply #'format nil format-args)
            (ensure-directory-pathname
             (sb-posix:getenv "HOME")))))
