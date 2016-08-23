(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(
               :with-gensyms
               :once-only
               :compose
               :curry
               :rcurry
               :define-constant
               :map-product
               :map-tree
               :equivalence-classes
               :ensure-gethash
               ; :n-grams
               ; :switch
               ; :while
               ; :ensure-boolean
               ; :iota
               ; :zip
               )
  :package "HYPE.QUICKUTILS")
