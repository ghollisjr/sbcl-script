(in-package :sbcl-script)

(defun script-p ()
  (member :script *features*))
