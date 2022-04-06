#!/usr/bin/env -S sbcl --core ../sbcl-core/sbcl-script.core --script
(unless (member :script *features*)
  (ql:quickload :sbcl-script))
(in-package :sbcl-script)

(defun pipe-example ()
  "Shows how to use the #'pipe function."
  ;; Show the file sizes of the 3 biggest files and the total size
  (pipe `(("ls" ("-l"))
          ("awk" ("{print $5, $9}"))
          ("sort" ("-n"))
          ("tail" ("-n" "3"))
          ("awk"
           ("BEGIN {sum=0;} {sum+=$1; print $1, $2} END {print sum, \"total\"}")))))
