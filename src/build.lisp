(ql:quickload :cacl)

;; Run these generic functions once now so their bodies will get compiled at
;; build time, instead of delaying it until the first time the user runs
;; a command.  In SBCL at least, compiling the generic function for the first
;; time takes a noticeable amount of time (somewhere around a quarter of
;; a second), so let's not be annoying.

(cacl::command 'cacl::nop)
(cacl::command-documentation 'cacl::nop)

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
