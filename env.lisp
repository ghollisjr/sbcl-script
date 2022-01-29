(in-package :sbcl-script)

(defun script-p ()
  (member :sbcl-script *features*))
