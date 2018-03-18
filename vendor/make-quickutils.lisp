(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :once-only
               :with-gensyms
               :rcurry
               :make-gensym-list
               :ensure-list
               :range


               )
  :package "CACL.QUICKUTILS")
