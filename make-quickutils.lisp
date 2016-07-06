(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(
               :with-gensyms
               :once-only
               :compose
               :curry
               :rcurry
               :n-grams
               :define-constant
               ; :switch
               ; :while
               ; :ensure-boolean
               ; :iota
               ; :zip
               )
  :package "HYPE.QUICKUTILS")
