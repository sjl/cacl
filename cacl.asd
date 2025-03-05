(asdf:defsystem :cacl
  :description "RPN calculator in Common Lisp"
  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (:adopt
               :alexandria
               :losh
               :iterate
               :str
               :uiop)

  :serial t
  :components (
               (:module "src" :serial t
                :components ((:file "package")
                             (:file "base")
                             (:file "math")
                             (:file "units")))))
