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
               #:temperance
               #+sbcl #:sb-sprof)

  :serial t
  :components
  ((:module "vendor"
    :components ((:file "quickutils")))
   (:file "package")
   (:module "src"
    :serial t
    :components ((:file "utils")
                 (:file "main")
                 (:module "players"
                  :serial t
                  :components ((:file "random")))))))
