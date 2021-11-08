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
