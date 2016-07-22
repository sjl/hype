(in-package #:hype)


(defclass random-player (ggp:ggp-player)
  ((role :type symbol :accessor rp-role)
   (database :accessor rp-database)
   (state :accessor rp-state)))

(defvar *random-player* (make-instance 'random-player
                                       :name "Hype-Random"
                                       :port 5000))

(defmethod ggp:player-start-game ((player random-player) rules role timeout)
  (setf (rp-role player) role
        (rp-database player) (make-database))
  (with-database (rp-database player)
    (load-gdl-preamble)
    (push-logic-frame-with
      (load-rules rules))
    (setf (rp-state player) (initial-state))))

(defmethod ggp:player-stop-game ((player random-player))
  (setf (rp-state player) nil
        (rp-database player) nil
        (rp-role player) nil))

(defmethod ggp:player-update-game ((player random-player) moves)
  (when moves
    (with-database (rp-database player)
      (apply-state (rp-state player))
      (apply-moves moves)
      (setf (rp-state player) (next-state))
      (clear-moves)
      (clear-state))))

(defun random-nth (list)
  (nth (random (length list)) list))

(defmethod ggp:player-select-move ((player random-player) timeout)
  (with-database (rp-database player)
    (prog2
      (apply-state (rp-state player))
      (cdr (random-nth (legal-moves-for (rp-role player))))
      (clear-state))))

; (ggp:start-player *random-player*)
; (ggp:kill-player *random-player*)
