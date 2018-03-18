(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :ensure-list
               :make-gensym-list
               :once-only
               :parse-body
               :range
               :rcurry
               :with-gensyms


               )
  :package "CACL.QUICKUTILS")
