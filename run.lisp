(in-package :sbcl-script)

(defun buf->string (buf)
  (map 'string #'identity
       (reverse buf)))

(defun read-stream (stream)
  (let* ((result nil))
    (loop
      for c = (read-char stream nil nil)
      while c
      do (push c result))
    (buf->string result)))

(defun whitespace-p (c)
  (and (characterp c)
       (or (char= c #\Space)
           (char= c #\Tab)
           (char= c #\Newline))))

(defun butnewline (string)
  (let* ((last (elt string (1- (length string)))))
    (if (char= last #\newline)
        (subseq string 0 (1- (length string)))
        string)))

(define-condition run-error ()
  ((stderr :initarg :stderr :reader run-error-stderr)
   (stdout :initarg :stdout :reader run-error-stdout)))

(defun run (command &optional arguments
            &key
              (external-format :utf-8)
              show-output
              input)
  "Runs program synchronously, returning output string as first result
unleses show-output is non-NIL, in which case output is printed to
*standard-output*."
  (let* ((outstr "")
         (errstr "")
         (retval 0)
         (proc nil))
    (restart-case
        (progn
          (setf
           outstr
           (with-output-to-string (stdout)
             (setf
              errstr
              (with-output-to-string (stderr)
                (setf proc
                      (apply #'sb-ext:run-program
                             "/usr/bin/env"
                             (list* command
                                    arguments)
                             :external-format external-format ;; :iso-8859-1
                             :output (if show-output
                                         *standard-output*
                                         stdout)
                             :error (if show-output
                                        *error-output*
                                        stderr)
                             :wait nil
                             (when input
                               (list :input input))))
                (sb-ext:process-wait proc)
                (setf retval
                      (sb-ext:process-exit-code
                       proc))
                (sb-ext:process-close proc)))))
          (when (not (zerop retval))
            (error 'run-error :stderr errstr :stdout outstr))
          (values outstr errstr retval))
      (abort-run ()
        (sb-ext:process-kill proc 9 :process-group)
        (sb-ext:process-close proc))
      (ignore-error () (values outstr errstr retval)))))

(defmacro and-run (&rest runs)
  "Executes external processes until one returns a non-zero exit
code."
  )


;; Read macro for shell-like command execution
(defun run-reader-macro (stream subchar arg)
  (let* ((buffer nil)
         (start (read-char stream nil nil))
         (end
           (cond
             ((null start)
              #\))
             ((char= start #\()
              #\))
             ((char= start #\")
              #\")
             ((char= start #\')
              #\'))))
    (loop
      for c = (read-char stream nil nil)
      while (and c
                 (not (char= c end)))
      do (push c buffer))
    (let* ((str (buf->string buffer))
           (terms nil)
           (term nil)
           (command-list
             (loop
               for c across str
               do
                  (cond
                    ((whitespace-p c)
                     (when term
                       (push (buf->string term) terms)
                       (setf term nil)))
                    (t
                     (push c term)))
               finally (progn
                         (when term
                           (push (buf->string term) terms))
                         (return (reverse terms))))))
      (let* ((stdout (gensym "stdout"))
             (stdoutstr (gensym "stdoutstr"))
             (stderr (gensym "stderr"))
             (stderrstr (gensym "stderrstr")))
        `(run ,(first command-list)
              (list ,@(rest command-list)))))))

(defun enable-run-reader-macro ()
  (set-dispatch-macro-character
   #\# #\r #'run-reader-macro))

(defun read-entire-stream (stream)
  "Reads all characters from stream and returns string."
  (let* ((result nil))
    (loop
      for c = (read-char stream nil nil)
      while c
      do (dlist-push c result :at-end t))
    (coerce result 'string)))


(defun pipe (commands
             &key
               (external-format :utf-8)
               show-output
               input)
  "Executes chain of commands with inputs & outputs piped together.
Each command is of the form (program args &key stderr-p) where non-NIL
stderr-p results in the stderr output being combined with stdout
before piping.

NOTE: I have been unable to directly use pipes due to the way SBCL
handles program I/O.  It appears that sb-ext:run-program does not
gracefully handle a file descriptor closing while it reads from the
file descriptor, so trying to use a cl-plumbing::pipe causes an EOF
error if the pipe gets closed or an eternal hang since the pipe can't
be closed.  This causes woeful inefficiency for this pipe function,
but it's still useful for many tasks."
  (let* ((stdout "")
         (stderr "")
         (retval 0)
         (n (length commands))
         (proc nil))
    (when (= n 1)
      (return-from pipe
        (run (first (first commands))
             (second (first commands))
             :external-format external-format
             :show-output show-output
             :input input)))
    (loop
      for c in commands
      for i from 1
      do
         (with-input-from-string (si stdout)
           (setf stdout
                 (with-output-to-string (so)
                   (setf stderr
                         (with-output-to-string (se)
                           (destructuring-bind (program args &key stderr-p)
                               c
                             (setf proc
                                   (apply #'sb-ext:run-program
                                          "/usr/bin/env"
                                          (list*
                                           (list* program
                                                  args)
                                           :wait t
                                           :output
                                           (if  (and (= i n)
                                                     show-output)
                                                *standard-output*
                                                so)
                                           :external-format external-format
                                           (append
                                            (when (> i 1)
                                              (list :input si))
                                            (when (or stderr-p
                                                      (and show-output
                                                           (= i n)))
                                              (list :error se))))))
                             (setf retval
                                   (sb-ext:process-exit-code proc)))))))))
    (values stdout stderr retval)))
