(ql:quickload (list "sbcl-script"
                    "cl-plumbing"))

(in-package :sbcl-script)

;;;; These tests compare the performance of raw string I/O and
;;;; cl-plumbing pipes.  On my system, I get the following:
;;;;
;;;; Bash version: 0.01 seconds
;;;; (test): 0.073 seconds
;;;; (pipe-test): 0.246 seconds

(defun test ()
  (let* ((output nil))
    (setf output
          (with-output-to-string (s)
            (sb-ext:run-program "/usr/bin/env"
                                (list "ls")
                                :output s
                                :wait t)))
    (WITH-INPUT-FROM-STRING (s output)
      (sb-ext:run-program "/usr/bin/env"
                          (list "sort")
                          :wait t
                          :input s
                          :output *standard-output*))))

(defun pipe-test ()
  (let* ((pipe (cl-plumbing:make-pipe)))
    (sb-ext:run-program "/usr/bin/env"
                        (list "ls")
                        :output pipe
                        :wait t)
    (close pipe)
    (sb-ext:run-program "/usr/bin/env"
                          (list "sort")
                          :wait t
                          :input pipe
                          :output *standard-output*)))
