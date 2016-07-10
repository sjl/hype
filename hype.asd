(asdf:defsystem #:hype
  :name "hype"
  :description "HYPE"

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (#:defstar
               #:iterate
               #:cl-arrows
               #:cl-ggp
               #:bones
               #:sb-sprof
               )

  :serial t
  :components
  ((:file "quickutils") ; quickutils package ordering crap
   (:file "package")
   (:module "src"
    :serial t
    :components ((:file "utils")
                 (:file "main")
                 ))))
