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

(defun run (command arguments
            &key
              input)
  (let* ((outstr "")
         (errstr "")
         (retval 0))
    (setf
     outstr
     (with-output-to-string (stdout)
       (setf
        errstr
        (with-output-to-string (stderr)
          (setf retval
                (sb-ext:process-exit-code
                 (apply #'sb-ext:run-program
                        "/usr/bin/env"
                        (list* command
                               arguments)
                        :output stdout
                        :error stderr
                        (when input
                          (list :input input)))))))))
    (when (not (string= errstr ""))
      (error "run error: ~a" errstr))
    (values outstr errstr retval)))

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
