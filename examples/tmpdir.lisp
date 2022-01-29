#!/usr/bin/env -S sbcl --core ../sbcl-core/sbcl-script.core --script
;; require is not needed for script-only execution, but needed for
;; safe loading as non-script.  It adds a small delay to execution,
;; ~0.02 seconds on my system.
(require 'sbcl-script) 
(in-package :sbcl-script)

(when (script-p) ; Similar to Python's if __name__ == "__main__":
  ;; could have also done (when (member :script *features*) ...)
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
