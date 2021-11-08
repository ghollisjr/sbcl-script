sbcl-script is a Common Lisp library to make writing scripts easier
with SBCL.  If you like using a different Lisp implementation, you
might find some of this code useful.

I started this project with the goal of making an
implementation-agnostic scripting library but I ran into a dead end
when it came to a fully equipped external program execution library
(process group control & interrupts, pipes, async execution control
especially).  SBCL has these.  If your favorite implementation has
them too, I would much appreciate contributions and would happily
change this to an implementation agnostic library, maybe call it
something like cl-script.

Dependencies:

* quicklisp (https://www.quicklisp.org)
* external-program (https://github.com/sellout/external-program)
* cl-options (https://www.github.com/ghollisjr/cl-options)

Make sure you have SBCL configured for scripting, quicklisp, run
"make" and then "cd" into the examples/ directory if you want to try
out the examples.

To configure SBCL for scripting, see the manual or do something like
this in your .sbclrc file:

;;; If the first user-processable command-line argument is a filename,
;;;disable the debugger, load the file handling shebang-line and quit.
(let ((script (and (second *posix-argv*)
                   (probe-file (second *posix-argv*)))))
  (when script
    ;; Handle shebang-line
    (set-dispatch-macro-character #\# #\!
                                  (lambda (stream char arg)
                                    (declare (ignore char arg))
                                    (read-line stream)))
    ;; Disable debugger
    (setf *invoke-debugger-hook*
          (lambda (condition hook)
            (declare (ignore hook))
            ;; Uncomment to get backtraces on errors
            ;; (sb-debug:backtrace 20)
            (format *error-output* "Error: ~A~%" condition)
            (exit)))
    (load script)
    (exit)))
