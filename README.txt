sbcl-script is a Common Lisp library to make writing scripts easier
with SBCL.  If you like using a different Lisp implementation, you
might find some of this code useful.

The driving ideas of sbcl-script:

* Common Lisp out-of-the-box is not all that convenient for writing
  scripts that would e.g. replace or extend a shell script.

* With appropriate libraries and macros, Lisp scripts should be as
  easy or easier to write than shell scripts and should have better
  performance.

* The operating system exists, and Lisp benefits from knowing about
  it.

* make-sbcl-core: Building & installing Lisp binaries is less
  convenient than building C binaries, and would benefit from build
  tools that can be called outside of a Lisp image.  make-sbcl-core
  provides this via a convenient way of generating core files given a
  list of dependencies to load via quicklisp.

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

Make sure if you're using an old version of SBCL that you have it
configured for scripting (see below), quicklisp, run "make" and then
"cd" into the examples/ directory if you want to try out the examples.

The make-sbcl-core script makes use of the *features* variable to add
:script to the features list.  This can be checked directly or with
the #'script-p function, which checks for this feature.  If the only
place this feature is enabled is in cores, then you can exploit the
Python-like script-library duality for files.  E.g., in Python you
might do something like

if __name__ == "__main__":
   ...

at the bottom of a script that you also want to use as a library.
This would be accomplished like

#+script
(some code to run as script executable)

or equivalently

(when (member :script *features*) ; also (sbcl-script:script-p)
  ...)

if a make-sbcl-core core file were loaded.  See build-sbcl-core.sh for
an example of how to use make-sbcl-core.  I recommend placing
make-sbcl-core somewhere in your $PATH variable to make building
script cores easier.

In order to allow scripts to be loaded as ordinary Lisp files, you
will need to at least add this to your .sbclrc file:

----------------------------------------------------------------------
(set-dispatch-macro-character
 #\# #\!
 (lambda (stream char arg)
   (declare (ignore char arg))
   (read-line stream)))
----------------------------------------------------------------------

To configure SBCL for scripting if using a particularly old version of
SBCL, see the manual or do something like this in your .sbclrc file:

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
