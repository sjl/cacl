(ql:quickload :cacl)

#+sbcl
(progn
  (sb-ext:gc :full t)
  (sb-ext:save-lisp-and-die
    "cacl"
    :executable t
    :compression nil
    :toplevel #'cacl:toplevel
    :save-runtime-options t))

#+ccl
(progn
  (ccl:gc)
  (ccl:save-application
    "cacl"
    :toplevel-function #'cacl:toplevel
    :purify t
    :prepend-kernel t))
