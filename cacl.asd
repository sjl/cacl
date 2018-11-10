(asdf:defsystem :cacl
  :description "RPN calculator in Common Lisp"
  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (:losh
               :drakma
               :flexi-streams
               :iterate
               :str
               :yason)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "base")
                             (:file "json")
                             (:file "math")))))
