;; Copyright 2012 Ravi Desai
;; Distributed under the GNU Affero General Public License version 3 or later.

(in-package :dice-game)

(defparameter die1 nil)
(defparameter die2 nil)

(defun roll-dice ()
  (setf die1 (1+ (random 6)))
  (setf die2 (1+ (random 6))))

(defun score ()
  (roll-dice)
  (let ((score (cond
		 ((or (= 1 die1)
		      (= 1 die2))
		  0)
		 ((= die1 die2)
		  (* 2
		     (+ die1 die2)))
		 (t
		  (+ die1 die2)))))
    (list die1 die2 score)))
