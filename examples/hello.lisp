#!/usr/bin/env -S sbcl --core ../sbcl-core/sbcl-script.core --script
#-script (ql:quickload :sbcl-script)
(in-package :sbcl-script)

(defun hello ()
  (format t "Hello, World!~%"))

#+script (hello)
