#!/usr/bin/env -S sbcl --core ../sbcl-core/sbcl-script.core --script
#-script (ql:quickload :sbcl-script)

(in-package :sbcl-script)

(defun tmpdir-test ()
  (with-temp (tmpdir t)
    (with-open-file (f (tmppath "test.txt")
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (format t "What's in the temp dir?~%~a"
              (run "ls" (list (namestring tmpdir))))
      (format f "Now you see me~%"))
    (format t "What's in the new file?~%~a"
            (run "cat" (list (namestring (tmppath "test.txt"))))))
  (format t "Now you don't~%"))

#+script ; Similar to Python's if __name__ == "__main__":
(tmpdir-test)
