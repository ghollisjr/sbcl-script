#!/bin/bash
mkdir -p sbcl-core/
echo '(ql:quickload "sbcl-script") (sb-ext:save-lisp-and-die "sbcl-core/sbcl-script.core")' | sbcl
