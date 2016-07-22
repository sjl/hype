(in-package #:hype)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
; (declaim (optimize (speed 0) (debug 3) (safety 3)))


(asdf:load-system 'bones :force t)

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream))
      )
  (asdf:load-system 'bones :force t))


;;;; Unique Consing
;;; Originally from PAIP, but updated with bugfix and macros.

(defparameter *cons-table* (make-hash-table :test 'eql))
(defparameter *atom-table* (make-hash-table :test 'eql))


(defmacro with-fresh-cons-pool (&body body)
  `(let ((*cons-table* (make-hash-table :test 'eql))
         (*atom-table* (make-hash-table :test 'equal)))
    ,@body))

(defmacro gethash-defaulted (key hash-table &body body)
  "Get `key` from `hash-table`.

  If `key` is not present, `body` will be evaluated, inserted into the table at
  `key`, and returned.

  "
  (once-only (key hash-table)
    (with-gensyms (result found)
      `(multiple-value-bind (,result ,found) (gethash ,key ,hash-table)
         (if ,found
           ,result
           (setf (gethash ,key ,hash-table)
                 (progn ,@body)))))))

(defmacro short-circuiting-get (v uv table init)
  (once-only (v table)
    (with-gensyms (result found)
      `(multiple-value-bind (,result ,found) (gethash ,v ,table)
        (if ,found
          (progn (setf ,uv ,v)
                 ,result)
          (progn (setf ,uv (unique ,v))
                 (gethash-defaulted ,uv ,table ,init)))))))


(defun unique-cons (a b &aux ua ub)
  (-<> *cons-table*
    (short-circuiting-get a ua <> (make-hash-table :test 'eql))
    (short-circuiting-get b ub <> (cons ua ub))))

(defun unique (form)
  (etypecase form
    (symbol form)
    (number form)
    (atom (gethash-defaulted form *atom-table* form))
    (cons (unique-cons (car form) (cdr form)))))

(defun ulist (&rest args)
  (unique args))

(defun uappend (a b)
  (if (null a)
    (unique b)
    (unique-cons (car a) (uappend (cdr a) b))))


;;;; State Trie
;;; Based on the PAIP tries.

(defconstant +trie-deleted+ 'trie-deleted)
(defconstant +trie-cons+ 'trie-cons)


(defstruct trie
  (arcs nil :type list)
  (value nil :type t))


(defun* follow-arc ((trie trie) arc-key extend?)
  ;; returns (node did-extend)
  (let ((arc (assoc arc-key (trie-arcs trie))))
    (cond
      (arc (values (cdr arc) nil))
      (extend? (let ((new-trie (make-trie)))
                 (push (cons arc-key new-trie)
                       (trie-arcs trie))
                 (values new-trie t)))
      (t (values nil nil)))))

(defun* find-leaf (trie key &optional extend?)
  ;; returns (node did-extend)
  (cond
    ((null trie) (values nil nil))
    ((atom key) (follow-arc trie key extend?))
    (t (-> trie
         (follow-arc +trie-cons+ extend?)
         (find-leaf (car key) extend?)
         (find-leaf (cdr key) extend?)))))


(defun* get-trie (key (trie trie) &optional default)
  (let* ((leaf (find-leaf trie key))
         (result (when leaf (trie-value leaf))))
    (if (or (null leaf) (eq result +trie-deleted+))
      (values default nil)
      (values result t))))

(defun* (setf get-trie) (new-value key trie)
  (setf (trie-value (find-leaf trie key t)) new-value))

(defun* rem-trie (key trie)
  (setf (get-trie key trie) +trie-deleted+))


(defun trie-to-cons (trie)
  (recursively ((trie trie))
    (list (list :value (trie-value trie))
          (list* :arcs (loop :for (key . child) :in (trie-arcs trie)
                            :collect (list key '-> (recur child)))))))

(defmethod print-object ((trie trie) stream)
  (print-unreadable-object (trie stream)
    (let ((*print-pretty* t))
      (prin1 (trie-to-cons trie) stream))))


;;;; GDL
(defun read-gdl (filename)
  (let ((*package* (find-package :ggp-rules)))
    (with-open-file (stream filename)
      (loop
        :with done = (gensym)
        :for form = (read stream nil done)
        :while (not (eq form done))
        :collect form))))

(defun load-rules (gdl)
  (mapc (lambda (rule)
          (if (and (consp rule)
                   (eq (car rule) 'ggp-rules::<=))
            (apply #'invoke-rule (cdr rule))
            (invoke-fact rule)))
        gdl))

(defun load-gdl (filename)
  (push-logic-frame-with
    (load-rules (read-gdl filename))))

(defun load-gdl-preamble ()
  (push-logic-frame-with
    (rule (ggp-rules::not ?x) (call ?x) ! fail)
    (fact (ggp-rules::not ?x))

    (rule (ggp-rules::or ?x ?y) (call ?x))
    (rule (ggp-rules::or ?x ?y) (call ?y))

    (rule (ggp-rules::distinct ?x ?x) ! fail)
    (fact (ggp-rules::distinct ?x ?y))))

(defun initialize-database (filename)
  (setf bones.wam::*database* (make-database))
  (load-gdl-preamble)
  (load-gdl filename)
  (values))


;;;; GGP
(defun dedupe-state (state)
  (iterate (for fact :in state)
           (for prev :previous fact)
           (when (not (eql fact prev))
             (collect fact))))

(defun fact-slow< (a b)
  ;; numbers < symbols < conses
  (etypecase a
    (number (etypecase b
              (number (< a b))
              (t t)))
    (symbol (etypecase b
              (number nil)
              (cons t)
              (symbol (string< (symbol-name a) (symbol-name b)))))
    (cons (etypecase b
            (cons (cond
                    ((fact-slow< (car a) (car b)) t)
                    ((fact-slow< (car b) (car a)) nil)
                    (t (fact-slow< (cdr a) (cdr b)))))
            (t nil)))))

(defun fact< (a b)
  (if (eql a b)
    nil
    (let ((ha (sxhash a))
          (hb (sxhash b)))
      (if (= ha hb)
        (fact-slow< a b)
        (< ha hb)))))

(defun sort-state (state)
  (sort state #'fact<))

(defun normalize-state (state)
  (unique (dedupe-state (sort-state state))))


(defun initial-state ()
  (normalize-state
    (query-map (lambda (r) (getf r '?what))
               (ggp-rules::init ?what))))


(defun terminalp ()
  (prove ggp-rules::terminal))

(defun move= (move1 move2)
  (and (eq (car move1) (car move2))
       (eq (cdr move1) (cdr move2))))

(defun move-role= (move1 move2)
  (eq (car move1) (car move2)))

(defun legal-moves-for (role)
  (invoke-query-map (lambda (move)
                      (cons (getf move '?role)
                            (getf move '?action)))
                    `(ggp-rules::legal ,role ?action)))

(defun legal-moves ()
  (let* ((individual-moves
           (remove-duplicates
             (query-map (lambda (move)
                          (cons (getf move '?role)
                                (getf move '?action)))
                        (ggp-rules::legal ?role ?action))
             :test #'move=))
         (player-moves
           (equivalence-classes #'move-role= individual-moves))
         (joint-moves
           (apply #'map-product #'list player-moves)))
    joint-moves))

(defun roles ()
  (query-map (lambda (r) (getf r '?role))
             (ggp-rules::role ?role)))

(defun goal-value (role)
  (getf (invoke-query `(ggp-rules::goal ,role ?goal))
        '?goal))

(defun goal-values ()
  (invoke-query-all `(ggp-rules::goal ?role ?goal)))

(defun next-state ()
  (normalize-state
    (query-map (lambda (r) (unique (getf r '?what)))
               (ggp-rules::next ?what))))


(defun apply-state (state)
  (push-logic-frame-with
    (loop :for fact :in state
          :do (invoke-fact `(ggp-rules::true ,fact)))))

(defun apply-moves (moves)
  (push-logic-frame-with
    (loop :for (role . action) :in moves
          :do (invoke-fact `(ggp-rules::does ,role ,action)))))


(defun clear-state ()
  (pop-logic-frame))

(defun clear-moves ()
  (pop-logic-frame))


;;;; Cache
(defvar *cache* nil)
(defvar *cache-hits* nil)
(defvar *cache-misses* nil)

(declaim (inline get-cached-result))

(defun get-cached-result (getter-function state remaining-depth)
  (multiple-value-bind (result found) (funcall getter-function state *cache*)
    (if (not found)
      (values nil nil)
      (destructuring-bind (goal . subtree-depth) result
        (cond
          ((eq t subtree-depth) (values goal t)) ; if the cached result is from hitting a terminal
          ((<= remaining-depth subtree-depth) (values goal t))
          (t (values nil nil)))))))

(defmacro get-cached-or ((getter state remaining-depth) &body body)
  (once-only (state remaining-depth)
    (with-gensyms (found result)
      `(multiple-value-bind (,result ,found)
        (get-cached-result (function ,getter) ,state ,remaining-depth)
        (if ,found
          (progn
            (incf *cache-hits*)
            ,result)
          (progn
            (incf *cache-misses*)
            (setf (,getter ,state *cache*) (progn ,@body))))))))


;;;; Search
(defvar *count* 0)
(defvar *role* nil)


(defun children ()
  (iterate
    (for joint-move :in (legal-moves))
    (collect (prog2 (apply-moves joint-move)
                    (cons joint-move (next-state))
                    (clear-moves)))))


(defun dfs (state path remaining-depth)
  (labels ((handle-terminal ()
             (cons (if (= 100 (goal-value *role*))
                     (list state (reverse path))
                     nil)
                   t)))
    (when (zerop (mod (incf *count*) 10000))
      (format t "~D...~%" *count*))
    (get-cached-or (gethash state remaining-depth)
      (apply-state state)
      (cond
        ((terminalp)
         (prog1
             (handle-terminal)
           (clear-state)))

        ((zerop remaining-depth)
         (clear-state)
         (cons nil 0))

        (t
         (iterate
           (with next = (children))
           (with finished = t)
           (initially (clear-state))
           (for (move . next-state) :in next)
           (for (result . subtree-depth) = (dfs next-state
                                                (cons move path)
                                                (1- remaining-depth)))
           (unless (eq subtree-depth t)
             (setf finished nil))
           (when result
             (leave (cons result nil)))
           (finally (return (cons nil (if finished
                                        t
                                        remaining-depth))))))))))

(defun depth-first-search (limit)
  (dfs (initial-state) nil limit))


(defun iterative-deepening-search (absolute-limit)
  (iterate
    (for limit :from 1 :to absolute-limit)
    (format t "~%Searching depth ~D~%" limit)
    (thereis (car (depth-first-search limit)))
    (format t "Cache: (size ~D) (hits ~D) (misses ~D)~%"
            ; 0
            (hash-table-count *cache*)
            *cache-hits* *cache-misses*)))


(defun run-game (filename limit)
  (initialize-database filename)
  (let ((*count* 0)
        (*role* (car (roles)))
        (*cache* (make-hash-table :test 'eq :size 10000))
        ; (*cache* (make-trie))
        (*cache-hits* 0)
        (*cache-misses* 0))
    (let ((result (with-fresh-cons-pool
                    (iterative-deepening-search limit))))
      (if (null result)
        (format t "~%No solution found.~%")
        (let ((*package* (find-package :ggp-rules)))
          (format t "~%Final state:")
          (pprint (first result))
          (format t "~2%Path:")
          (map nil #'pprint (second result)))))))


;;;; Wat
; (initialize-database "gdl/tictactoe.gdl")

;;;; Profiling
#+sbcl
(defun profile (game limit)
  ; (declare (optimize (speed 1) (debug 1) (safety 1)))
  (sb-ext:gc :full t)
  (require :sb-sprof)
  (sb-sprof::profile-call-counts "HYPE")
  (sb-sprof::profile-call-counts "BONES.WAM")
  (sb-sprof::profile-call-counts "BONES.CIRCLE")
  (sb-sprof::with-profiling (:max-samples 50000
                             :reset t
                             ; :mode :alloc
                             :mode :cpu
                             :sample-interval 0.006
                             :alloc-interval 1)
    (time (run-game game limit)))
  (with-open-file (*standard-output* "hype.prof"
                                     :direction :output
                                     :if-exists :supersede)
    (sb-sprof:report :type :graph
                     :sort-by :cumulative-samples
                     :sort-order :ascending)
    (sb-sprof:report :type :flat
                     :min-percent 0.5)))


;;;; Scratch
; (profile "gdl/hanoi.gdl" 33)
; (profile "gdl/aipsrovers01.gdl" 11)
; (sb-sprof:report :type :flat :min-percent 0.5)
; (sb-sprof:report :type :flat :sort-by :cumulative-samples :sort-order :ascending)
; (time (run-game "gdl/hanoi.gdl" 33))
; (time (run-game "gdl/aipsrovers01.gdl" 11))
; (time (run-game "gdl/8puzzle.gdl" 21))
