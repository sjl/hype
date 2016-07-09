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
  (sort-state (dedupe-state state)))


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


;;;; Search
(defvar *count* 0)
(defvar *role* nil)

(defun children ()
  (iterate
    (for joint-move :in (legal-moves))
    (collect (prog2 (apply-moves joint-move)
                    (cons joint-move (next-state))
                    (clear-moves)))))


(defun depth-first-search (state path limit)
  (when (zerop (mod (incf *count*) 1000))
    (format t "~D...~%" *count*))
  (when (plusp limit)
    (apply-state state)
    (if (terminalp)
      (prog1
          (if (eq 'num-100 (goal-value *role*))
            (list state (reverse path))
            nil)
        (clear-state))
      (iterate
        (with next = (children))
        (initially (clear-state))
        (for (move . next-state) :in next)
        (thereis (depth-first-search next-state
                                     (cons move path)
                                     (1- limit)))))))


(defun iterative-deepening-search (absolute-limit)
  (iterate
    (for limit :from 4 :to absolute-limit)
    (format t "~%Searching depth ~D~%" limit)
    (thereis (depth-first-search (initial-state) nil limit))))

(defun run-game (filename)
  (initialize-database filename)
  (let ((*count* 0)
        (*role* (car (roles))))
    (iterative-deepening-search 10)))


(run-game "gdl/buttons.gdl")
; (run-game "gdl/aipsrovers01.gdl")
