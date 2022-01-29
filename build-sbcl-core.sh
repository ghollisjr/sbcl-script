#!/bin/bash
mkdir -p sbcl-core/
./make-sbcl-core '(:sbcl-script)' "sbcl-core/sbcl-script.core"
#echo '(ql:quickload "sbcl-script") (sb-ext:save-lisp-and-die "sbcl-core/sbcl-script.core")' | sbcl
