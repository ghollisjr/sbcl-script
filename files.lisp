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
code body."
  (let* ((result (gensym "result")))
    `(let* ((,path (mktemp ,directory-p))
            (,result (progn ,@body)))
       ,(if directory-p
            `(cl-fad:delete-directory-and-files ,path)
            `(delete-file ,path))
       ,result)))
