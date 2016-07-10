(in-package #:hype)

(declaim (optimize (speed 3) (debug 0) (safety 0)))
; (declaim (optimize (speed 0) (debug 3) (safety 3)))


;;;; Unique Consing (see PAIP)
(defparameter *cons-table* (make-hash-table :test 'eql))
(defparameter *atom-table* (make-hash-table :test 'eql))

(defmacro with-fresh-cons-pool (&body body)
  `(let ((*cons-table* (make-hash-table :test 'eql))
         (*atom-table* (make-hash-table :test 'equal)))
    ,@body))

(defmacro gethash-defaulted (key hash-table &body body)
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
      `(multiple-value-bind (,result ,found)
        (gethash ,v ,table)
        (if ,found
          (progn (setf ,uv ,v)
                 ,result)
          (progn (setf ,uv (unique ,v))
                 (gethash-defaulted ,uv ,table ,init)))))))

(defun unique (form)
  (etypecase form
    (symbol form)
    (number form)
    (atom (gethash-defaulted form *atom-table* form))
    (cons (unique-cons (car form) (cdr form)))))

(defun unique-cons (a b &aux ua ub)
  (-<> *cons-table*
    (short-circuiting-get a ua <> (make-hash-table :test 'eql))
    (short-circuiting-get b ub <> (cons ua ub))))

(defun ulist (&rest args)
  (unique args))

(defun uappend (a b)
  (if (null a)
    (unique b)
    (unique-cons (car a) (uappend (cdr a) b))))


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
           (when (not (eql fact prev))
             (collect fact))))

(defun fact-slow< (a b)
  ;; numbers < symbols < conses
  (etypecase a
    (symbol (etypecase b
              (number nil)
              (cons t)
              (symbol (string< (symbol-name a) (symbol-name b)))))
    (cons (etypecase b
            (cons (cond
                    ((fact-slow< (car a) (car b)) t)
                    ((fact-slow< (car b) (car a)) nil)
                    (t (fact-slow< (cdr a) (cdr b)))))
            (t nil)))
    (number (etypecase b
              (number (< a b))
              (t t)))))

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
  (unique (query-map (rcurry #'getf '?what)
                     (init ?what))))

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
    (query-map (lambda (r) (unique (getf r '?what)))
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
    (format t "Done.~%")
    (format t "Cache: (size ~D) (hits ~D) (misses ~D)~%"
            (hash-table-count *cache*)
            *cache-hits* *cache-misses*)))


(defun run-game (filename limit)
  (initialize-database filename)
  (let ((*count* 0)
        (*role* (car (roles)))
        (*cache* (make-hash-table :test 'eq :size 10000))
        (*cache-hits* 0)
        (*cache-misses* 0))
    (with-fresh-cons-pool
      (prog1
          (iterative-deepening-search limit)
        (format t "Final cons pool size: ~D~%" (hash-table-count *cons-table*))
        (format t "Final atom pool size: ~D~%" (hash-table-count *atom-table*))))))


;;;; Profiling
(defun profile (game limit)
  (require :sb-sprof)
  (sb-sprof::profile-call-counts "HYPE")
  (sb-sprof::with-profiling (:max-samples 4000
                            :reset t
                            :sample-interval 0.001)
    (time (run-game game limit))))


;;;; Scratch
; (sb-sprof:report :type :flat :sort-by :cumulative-samples :sort-order :ascending)
; (sb-sprof:report :type :flat :min-percent 1)
; (profile "gdl/hanoi.gdl" 33)



