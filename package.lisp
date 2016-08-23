(defpackage #:hype.utils
  (:use
    #:cl
    #:defstar
    #:iterate
    #:cl-arrows
    #:hype.quickutils)
  (:export
    #:zap%
    #:%
    #:recursively
    #:recur
    #:dis

    #:hash-set
    #:make-set
    #:set-contains-p
    #:set-add
    #:set-remove
    #:set-add-all
    #:set-remove-all
    #:set-random
    #:set-pop
    #:set-empty-p
    #:set-clear

    #:averaging
    #:timing
    #:real-time
    #:run-time
    #:since-start-into
    #:per-iteration-into

    #:queue
    #:queue-contents
    #:enqueue
    #:dequeue
    #:queue-empty-p
    #:queue-append

    )
  (:shadowing-import-from #:cl-arrows
    #:->))

(defpackage #:hype
  (:use
    #:cl
    #:defstar
    #:iterate
    #:cl-arrows
    #:temperance
    #:hype.quickutils
    #:hype.utils)
  (:export

    )
  (:shadowing-import-from #:cl-arrows
    #:->))
