(in-package :sbcl-script)

(defun safe-string (string)
  "Removes hash symbols, #, from strings so that no read time
execution is allowed on reading."
  (remove #\# string))

(defun safe-read-from-string (string &rest read-args)
  "Applies safe-string to the string and then applies read-from-string
to string and the read-args that follow."
  (apply #'read-from-string
         (safe-string string)
         read-args))
