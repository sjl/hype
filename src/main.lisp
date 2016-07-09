(in-package #:hype)

;;;; GDL
(defun read-gdl (filename)
  (with-open-file (stream filename)
    (loop
      :with done = (gensym)
      :for form = (read stream nil done)
      :while (not (eq form done))
      :collect form)))

(defun munge-numbers (gdl)
  (map-tree (lambda (f)
              (if (numberp f)
                (intern (format nil "NUM-~D" f))
                f))
            gdl))

(defun load-rules (gdl)
  (mapc (lambda (rule)
          (if (and (consp rule)
                   (eq (car rule) '<=))
            (apply #'invoke-rule (cdr rule))
            (invoke-fact rule)))
        gdl))

(defun load-gdl (filename)
  (push-logic-frame-with
    (-> (read-gdl filename)
      munge-numbers
      load-rules)))

(defun load-gdl-preamble ()
  (push-logic-frame-with
    (rule (not ?x) (call ?x) ! fail)
    (fact (not ?x))

    (rule (distinct ?x ?x) ! fail)
    (fact (distinct ?x ?y))))

(defun initialize-database (filename)
  (setf bones.wam::*database* (make-database))
  (load-gdl-preamble)
  (load-gdl filename)
  (values))


;;;; GGP
(defun dedupe-state (state)
  (iterate (for fact :in state)
           (for prev :previous fact)
           (when (not (equal fact prev))
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
  (let ((ha (sxhash a))
        (hb (sxhash b)))
    (if (= ha hb)
      (if (eql a b)
        nil
        (fact-slow< a b))
      (< ha hb))))

(defun sort-state (state)
  (sort state #'fact-slow<))

(defun normalize-state (state)
  (dedupe-state (sort-state state)))


(defun initial-state ()
  (query-map (rcurry #'getf '?what)
             (init ?what)))

(defun terminalp ()
  (prove terminal))

(defun move= (move1 move2)
  (and (eq (car move1) (car move2))
       (eq (cdr move1) (cdr move2))))

(defun move-role= (move1 move2)
  (eq (car move1) (car move2)))

(defun legal-moves-for (role)
  (invoke-query-map (lambda (move)
                      (cons (getf move '?role)
                            (getf move '?action)))
                    `(legal ,role ?action)))

(defun legal-moves ()
  (let* ((individual-moves
           (remove-duplicates
             (query-map (lambda (move)
                          (cons (getf move '?role)
                                (getf move '?action)))
                        (legal ?role ?action))
             :test #'move=))
         (player-moves
           (equivalence-classes #'move-role= individual-moves))
         (joint-moves
           (apply #'map-product #'list player-moves)))
    joint-moves))

(defun roles ()
  (query-map (lambda (r) (getf r '?role))
             (role ?role)))

(defun goal-value (role)
  (getf (invoke-query `(goal ,role ?goal))
        '?goal))

(defun goal-values ()
  (invoke-query-all `(goal ?role ?goal)))

(defun next-state ()
  (normalize-state
    (query-map (lambda (r) (getf r '?what))
               (next ?what))))


(defun apply-state (state)
  (push-logic-frame-with
    (loop :for fact :in state
          :do (invoke-fact `(true ,fact)))))

(defun apply-moves (moves)
  (push-logic-frame-with
    (loop :for (role . action) :in moves
          :do (invoke-fact `(does ,role ,action)))))


(defun clear-state ()
  (pop-logic-frame))

(defun clear-moves ()
  (pop-logic-frame))


;;;; Cache
(defvar *cache* nil)
(defvar *cache-hits* nil)
(defvar *cache-misses* nil)

(defun get-cached-result (state remaining-depth)
  (multiple-value-bind (result found) (gethash state *cache*)
    (if (not found)
      (values nil nil)
      (destructuring-bind (goal . subtree-depth) result
        (cond
          ((eq t subtree-depth) (values goal t)) ; if the cached result is from hitting a terminal
          ((<= remaining-depth subtree-depth) (values goal t))
          (t (values nil nil)))))))

(defmacro get-cached-or ((state remaining-depth) &body body)
  (once-only (state remaining-depth)
    (with-gensyms (found result)
      `(multiple-value-bind (,result ,found)
        (get-cached-result ,state ,remaining-depth)
        (if ,found
          (progn
            (incf *cache-hits*)
            ,result)
          (progn
            (incf *cache-misses*)
            (setf (gethash ,state *cache*) (progn ,@body))))))))


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
             (cons (if (eq 'num-100 (goal-value *role*))
                     (list state (reverse path))
                     nil)
                   t)))
    (when (zerop (mod (incf *count*) 10000))
      (format t "~D...~%" *count*))
    (get-cached-or (state remaining-depth)
      (if (zerop remaining-depth)
        (cons nil 0)
        (progn
          (apply-state state)
          (if (terminalp)
            (prog1
                (handle-terminal)
              (clear-state))
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
                                           remaining-depth)))))))))))

(defun depth-first-search (limit)
  (dfs (initial-state) nil limit))


(defun iterative-deepening-search (absolute-limit)
  (iterate
    (for limit :from 1 :to absolute-limit)
    (format t "~%Searching depth ~D~%" limit)
    (thereis (car (depth-first-search limit)))
    (format t "Done.~%")
    (format t "Cache: (size ~D) (hits ~D) (misses ~D)~%"
            (hash-table-count *cache*)
            *cache-hits* *cache-misses*)))


(defun run-game (filename)
  (initialize-database filename)
  (let ((*count* 0)
        (*role* (car (roles)))
        (*cache* (make-hash-table :test 'equal))
        (*cache-hits* 0)
        (*cache-misses* 0))
    (iterative-deepening-search 11)))


; (declaim (optimize (speed 3) (debug 0) (safety 0)))
(declaim (optimize (speed 0) (debug 3) (safety 3)))
; (run-game "gdl/buttons.gdl")


; (require :sb-sprof)
; (defun profile ()
;   (sb-sprof:with-profiling (:max-samples 4000
;                             :reset t
;                             :sample-interval 0.0001)
;     (time (run-game "gdl/aipsrovers01.gdl"))
;     ; (run-game "gdl/buttons.gdl")
;     ))
; (sb-sprof:profile-call-counts "HYPE")
; (profile)
; (sb-sprof:report :type :flat :sort-by :cumulative-samples :sort-order :ascending)
; (sb-sprof:report :type :flat :min-percent 3)
; (run-game "gdl/hanoi.gdl")
; (run-game "gdl/hanoi.gdl")
; (run-game "gdl/aipsrovers01.gdl")
