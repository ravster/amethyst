;; Copyright 2011-2012 Ravi Desai <rd7190@gmail.com>
;; Distributed under the terms of the GNU Affero GPL version 3 or any later version.

(in-package :random-entry)

;; Make the bar-array *array*.  This is hardcoded since its in the run-simulations-with-r function, and I don't want to change that right now.
(defparameter *array* (make-array 4000 :element-type 'bar :adjustable t :fill-pointer 0))

(defun process-data (raw-data data-vector)
  (read-ohlc raw-data data-vector)
  (calculate-true-range data-vector)
  (calculate-atr data-vector 20))

(defparameter *array-foo* (make-array '(10000 8))
  "Array that holds the values of interest with regards to a run of a trading system in each row.")

(defun position-size (dollar-amount-willing-to-be-risked number-of-pips-risked)
  "Return the position-size that must be taken, given the dollar amount we are willing to risk and the dollar amount that would have been risked with one full contract."
  (/ dollar-amount-willing-to-be-risked ;1 pip for 1 lot = $10.  4 decimal points in the data, not 5.
     (* number-of-pips-risked 100000)))	;Evaluates the the dollar-amount being risked, on GBPUSD & EURUSD pairs.

;; The point behind making this a macro is to reduce typing the same thing twice.  I tried doing this in the form of a function, but the results were coming out differently.  I should/will try it again in the future to see if there is something I didn't do correctly.
;; From the Toronto Lisp Users Group: I should consider turning this into an inline function.
(defmacro close-trade ()
  "This macro inserts the code that is required to simulate the closing of a trade."
  '(progn
    (if (> change 0)
	(progn
	  (incf num-win)
	  (incf amount-win change))
	(progn
	  (incf num-lose)
	  (incf amount-lose change)))

    ;;Over here we are building up the R-values in terms of pips.  The R-value of each trade is the ratio of the realization of profit (in pips) to the 1R (in pips).
    (incf R-value-sum-pips (/ change number-of-pips-risked))

    (let ((change-in-dollars (* change 
				100000	;Since "change" is in decimal, and not actually the number of pips.
				(position-size (/ capital 200) number-of-pips-risked))))
      (incf amount-of-dollars-won change-in-dollars)
      (incf capital change-in-dollars)
      (if (< change-in-dollars 0)
	  (setf minimum-account-balance (min minimum-account-balance
					     capital)))

      ;; Over here we are building up the R-values in terms of dollars.  This R-distribution is affected by the position-sizing algorithm.
      (incf R-value-sum-dollars (/ change-in-dollars R-in-dollars)))))

(defun run-simulations-with-r (simulations start-data-point end-data-point)
  "Random-entry system that also calculates the R-expectancies for each run."
  (dotimes (number-of-simulations simulations)
    ;;;The following happens for each simulation.
    (do ((i start-data-point (1+ i))
	 (length-array end-data-point)
	 (opening-price nil)
	 (trailing-stop)
	 (num-win 0)			;Number of winning trades in this simulation.
	 (amount-win 0)			;Number of winning pips.
	 (num-lose 0)			;Number of losing trades.
	 (amount-lose 0)		;Number of losing pips.
	 (number-of-pips-risked nil)	;1R in terms of pips.
	 (R-value-sum-pips 0)		;The sum of R-values in pips.
	 (amount-of-dollars-won 0)	;Net change in dollar-amount of the simulation.
	 (capital 10000)		;Initial capital in the account.
	 (minimum-account-balance 10000) ;Absolute draw-down of account-balance in simulation.
	 (R-in-dollars 0)		 ;The total amount risked per trade.
	 (R-value-sum-dollars 0) ;The sum of R-values (Realized profit / R) per trade in dollars.  This will be divided by the number of trades to get the R-expectancy for each run.
	 (order-type nil))
	((>= i (- length-array 7))
	 (setf (aref *array-foo* number-of-simulations 0) (+ amount-win amount-lose) ;Net pips won.
	       (aref *array-foo* number-of-simulations 1) amount-of-dollars-won      ;Net $ won.
	       (aref *array-foo* number-of-simulations 2) minimum-account-balance    ;Lowest account balance.
	       (aref *array-foo* number-of-simulations 3) num-win		     ;Number of winners.
	       (aref *array-foo* number-of-simulations 4) num-lose		     ;Number of losers.
	       (aref *array-foo* number-of-simulations 5) (float (/ num-win
								    (+ num-win num-lose))) ;Proportion of winning trades.
	       (aref *array-foo* number-of-simulations 6) (float (/ R-value-sum-dollars	   ;R-expectancy with position-sizing.
								    (+ num-win num-lose)))
	       (aref *array-foo* number-of-simulations 7) (float (/ R-value-sum-pips ;Pure R-expectancy.
								    (+ num-win num-lose)))))
      (if (eql opening-price nil)	;If no open trade.
	  ;; Entry rule & procedure.
	  (if (= 1 (random 2))
	      (progn			;On buy signal.
		(setf opening-price (openb (aref *array* (1+ i)))
		      trailing-stop (- opening-price
				       (* 3 (atrb (aref *array* i))))
		      order-type 1
		      number-of-pips-risked (- opening-price trailing-stop)
		      R-in-dollars (/ capital 200)))
	      (progn			;On sell signal.
		(setf opening-price (openb (aref *array* (1+ i)))
		      trailing-stop (+ opening-price
				       (* 3 (atrb (aref *array* i))))
		      order-type 0
		      number-of-pips-risked (- trailing-stop opening-price)
		      R-in-dollars (/ capital 200))))
	  ;; If open trade, exit or update the trailing stop.
	  (case order-type
	    (1				;If buy order.
	     (if (< (low (aref *array* i))
		    trailing-stop)	;Do we exit.  Only if we hit trailing stop.
		 (let ((change (- trailing-stop opening-price)))
		   (close-trade)
		   (setf opening-price nil trailing-stop nil number-of-pips-risked nil))
		 (setf trailing-stop	;If we have not yet hit the trailing stop.
		       (max trailing-stop
			    (- (high (aref *array* i))
			       (* 3 (atrb (aref *array* i))))))))
	    (0				;If sell order.
	     (if (> (high (aref *array* i))
		    trailing-stop)	;We exit if we hit the trailing stop.
		 (let ((change (- opening-price trailing-stop)))
		   (close-trade)
		   (setf opening-price nil trailing-stop nil number-of-pips-risked nil))
		 (setf trailing-stop	;If we have not yet hit the trailing stop.
		       (min trailing-stop
			    (+ (low (aref *array* i))
			       (* 3 (atrb (aref *array* i)))))))))))))

;; This function prints a 1- or 2-dimensional array onto a CSV-file.
(defun array-to-csv (array csv-file)
  "Print a <3-dimension array to a CSV-file."
  (with-open-file (stream csv-file :if-exists :overwrite :direction :output :if-does-not-exist :create)
    (if (eql (second (array-dimensions array))
	     nil)
	(dotimes (row-iterator (array-dimension array 0)) ;If only one dimension.
	  (format stream "~&~A" (aref array row-iterator)))
	(dotimes (row-iterator (array-dimension array 0)) ;If more than one dimension.
	  (let ((row ()))
	    (dotimes (column-iterator (array-dimension array 1))
	      (push (aref array row-iterator column-iterator) row))
	    (format stream "~&~{~A,~}" (reverse row)))))))
