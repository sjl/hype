;;;; This file was automatically generated by Quickutil.
;;;; See http://quickutil.org for details.

;;;; To regenerate:
;;;; (qtlc:save-utils-as "quickutils.lisp" :utilities '(:WITH-GENSYMS :ONCE-ONLY :COMPOSE :CURRY :RCURRY :DEFINE-CONSTANT :MAP-PRODUCT :MAP-TREE :EQUIVALENCE-CLASSES) :ensure-package T :package "HYPE.QUICKUTILS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "HYPE.QUICKUTILS")
    (defpackage "HYPE.QUICKUTILS"
      (:documentation "Package that contains Quickutil utility functions.")
      (:use #:cl))))

(in-package "HYPE.QUICKUTILS")

(when (boundp '*utilities*)
  (setf *utilities* (union *utilities* '(:STRING-DESIGNATOR :WITH-GENSYMS
                                         :MAKE-GENSYM-LIST :ONCE-ONLY
                                         :ENSURE-FUNCTION :COMPOSE :CURRY
                                         :RCURRY :DEFINE-CONSTANT :MAPPEND
                                         :MAP-PRODUCT :MAP-TREE
                                         :EQUIVALENCE-CLASSES))))

  (deftype string-designator ()
    "A string designator type. A string designator is either a string, a symbol,
or a character."
    `(or symbol string character))
  

  (defmacro with-gensyms (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(let ,(mapcar (lambda (name)
                     (multiple-value-bind (symbol string)
                         (etypecase name
                           (symbol
                            (values name (symbol-name name)))
                           ((cons symbol (cons string-designator null))
                            (values (first name) (string (second name)))))
                       `(,symbol (gensym ,string))))
            names)
       ,@forms))

  (defmacro with-unique-names (names &body forms)
    "Binds each variable named by a symbol in `names` to a unique symbol around
`forms`. Each of `names` must either be either a symbol, or of the form:

    (symbol string-designator)

Bare symbols appearing in `names` are equivalent to:

    (symbol symbol)

The string-designator is used as the argument to `gensym` when constructing the
unique symbol the named variable will be bound to."
    `(with-gensyms ,names ,@forms))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-gensym-list (length &optional (x "G"))
    "Returns a list of `length` gensyms, each generated as if with a call to `make-gensym`,
using the second (optional, defaulting to `\"G\"`) argument."
    (let ((g (if (typep x '(integer 0)) x (string x))))
      (loop repeat length
            collect (gensym g))))
  )                                        ; eval-when

  (defmacro once-only (specs &body forms)
    "Evaluates `forms` with symbols specified in `specs` rebound to temporary
variables, ensuring that each initform is evaluated only once.

Each of `specs` must either be a symbol naming the variable to be rebound, or of
the form:

    (symbol initform)

Bare symbols in `specs` are equivalent to

    (symbol symbol)

Example:

    (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
      (let ((y 0)) (cons1 (incf y))) => (1 . 1)"
    (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
          (names-and-forms (mapcar (lambda (spec)
                                     (etypecase spec
                                       (list
                                        (destructuring-bind (name form) spec
                                          (cons name form)))
                                       (symbol
                                        (cons spec spec))))
                                   specs)))
      ;; bind in user-macro
      `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
              gensyms names-and-forms)
         ;; bind in final expansion
         `(let (,,@(mapcar (lambda (g n)
                             ``(,,g ,,(cdr n)))
                           gensyms names-and-forms))
            ;; bind in user-macro
            ,(let ,(mapcar (lambda (n g) (list (car n) g))
                    names-and-forms gensyms)
               ,@forms)))))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; To propagate return type and allow the compiler to eliminate the IF when
  ;;; it is known if the argument is function or not.
  (declaim (inline ensure-function))

  (declaim (ftype (function (t) (values function &optional))
                  ensure-function))
  (defun ensure-function (function-designator)
    "Returns the function designated by `function-designator`:
if `function-designator` is a function, it is returned, otherwise
it must be a function name and its `fdefinition` is returned."
    (if (functionp function-designator)
        function-designator
        (fdefinition function-designator)))
  )                                        ; eval-when

  (defun compose (function &rest more-functions)
    "Returns a function composed of `function` and `more-functions` that applies its ;
arguments to to each in turn, starting from the rightmost of `more-functions`,
and then calling the next one with the primary value of the last."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (reduce (lambda (f g)
              (let ((f (ensure-function f))
                    (g (ensure-function g)))
                (lambda (&rest arguments)
                  (declare (dynamic-extent arguments))
                  (funcall f (apply g arguments)))))
            more-functions
            :initial-value function))

  (define-compiler-macro compose (function &rest more-functions)
    (labels ((compose-1 (funs)
               (if (cdr funs)
                   `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                   `(apply ,(car funs) arguments))))
      (let* ((args (cons function more-functions))
             (funs (make-gensym-list (length args) "COMPOSE")))
        `(let ,(loop for f in funs for arg in args
                     collect `(,f (ensure-function ,arg)))
           (declare (optimize (speed 3) (safety 1) (debug 1)))
           (lambda (&rest arguments)
             (declare (dynamic-extent arguments))
             ,(compose-1 funs))))))
  

  (defun curry (function &rest arguments)
    "Returns a function that applies `arguments` and the arguments
it is called with to `function`."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        ;; Using M-V-C we don't need to append the arguments.
        (multiple-value-call fn (values-list arguments) (values-list more)))))

  (define-compiler-macro curry (function &rest arguments)
    (let ((curries (make-gensym-list (length arguments) "CURRY"))
          (fun (gensym "FUN")))
      `(let ((,fun (ensure-function ,function))
             ,@(mapcar #'list curries arguments))
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest more)
           (apply ,fun ,@curries more)))))
  

  (defun rcurry (function &rest arguments)
    "Returns a function that applies the arguments it is called
with and `arguments` to `function`."
    (declare (optimize (speed 3) (safety 1) (debug 1)))
    (let ((fn (ensure-function function)))
      (lambda (&rest more)
        (declare (dynamic-extent more))
        (multiple-value-call fn (values-list more) (values-list arguments)))))
  

  (defun %reevaluate-constant (name value test)
    (if (not (boundp name))
        value
        (let ((old (symbol-value name))
              (new value))
          (if (not (constantp name))
              (prog1 new
                (cerror "Try to redefine the variable as a constant."
                        "~@<~S is an already bound non-constant variable ~
                       whose value is ~S.~:@>" name old))
              (if (funcall test old new)
                  old
                  (restart-case
                      (error "~@<~S is an already defined constant whose value ~
                              ~S is not equal to the provided initial value ~S ~
                              under ~S.~:@>" name old new test)
                    (ignore ()
                      :report "Retain the current value."
                      old)
                    (continue ()
                      :report "Try to redefine the constant."
                      new)))))))

  (defmacro define-constant (name initial-value &key (test ''eql) documentation)
    "Ensures that the global variable named by `name` is a constant with a value
that is equal under `test` to the result of evaluating `initial-value`. `test` is a
function designator that defaults to `eql`. If `documentation` is given, it
becomes the documentation string of the constant.

Signals an error if `name` is already a bound non-constant variable.

Signals an error if `name` is already a constant variable whose value is not
equal under `test` to result of evaluating `initial-value`."
    `(defconstant ,name (%reevaluate-constant ',name ,initial-value ,test)
       ,@(when documentation `(,documentation))))
  

  (defun mappend (function &rest lists)
    "Applies `function` to respective element(s) of each `list`, appending all the
all the result list to a single list. `function` must return a list."
    (loop for results in (apply #'mapcar function lists)
          append results))
  

  (defun map-product (function list &rest more-lists)
    "Returns a list containing the results of calling `function` with one argument
from `list`, and one from each of `more-lists` for each combination of arguments.
In other words, returns the product of `list` and `more-lists` using `function`.

Example:

    (map-product 'list '(1 2) '(3 4) '(5 6))
     => ((1 3 5) (1 3 6) (1 4 5) (1 4 6)
         (2 3 5) (2 3 6) (2 4 5) (2 4 6))"
    (labels ((%map-product (f lists)
               (let ((more (cdr lists))
                     (one (car lists)))
                 (if (not more)
                     (mapcar f one)
                     (mappend (lambda (x)
                                (%map-product (curry f x) more))
                              one)))))
      (%map-product (ensure-function function) (cons list more-lists))))
  

  (defun map-tree (function tree)
    "Map `function` to each of the leave of `tree`."
    (check-type tree cons)
    (labels ((rec (tree)
               (cond
                 ((null tree) nil)
                 ((atom tree) (funcall function tree))
                 ((consp tree)
                  (cons (rec (car tree))
                        (rec (cdr tree)))))))
      (rec tree)))
  

  (defun equivalence-classes (equiv seq)
    "Partition the sequence `seq` into a list of equivalence classes
defined by the equivalence relation `equiv`."
    (let ((classes nil))
      (labels ((find-equivalence-class (x)
                 (member-if (lambda (class)
                              (funcall equiv x (car class)))
                            classes))
               
               (add-to-class (x)
                 (let ((class (find-equivalence-class x)))
                   (if class
                       (push x (car class))
                       (push (list x) classes)))))
        (declare (dynamic-extent (function find-equivalence-class)
                                 (function add-to-class))
                 (inline find-equivalence-class
                         add-to-class))
        
        ;; Partition into equivalence classes.
        (map nil #'add-to-class seq)
        
        ;; Return the classes.
        classes)))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(with-gensyms with-unique-names once-only compose curry rcurry
            define-constant map-product map-tree equivalence-classes)))

;;;; END OF quickutils.lisp ;;;;