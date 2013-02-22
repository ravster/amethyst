;; Copyright 2012 Ravi Desai <rd7190@gmail.com>
;; Distributed under the terms of the GNU Affero GPL version 3 or any later version.

(in-package :20120329)

;; Make the bar-array
(defparameter *vector* (make-array 3000 :element-type 'bar :fill-pointer 0))

(defun process-data (raw-data data-vector)
  (read-ohlc raw-data data-vector)
  (calculate-true-range data-vector)
  (calculate-atr data-vector 20))

(defclass order ()
  (opening-price
   closing-price
   trailing-stop
   profit-$
   lot-size
   r-in-pips
   order-type))

(defun position-size (dollar-amount-willing-to-be-risked number-of-pips-risked)
  "Return the position-size that must be taken, given the dollar amount we are willing to risk and the dollar amount that would have been risked with one full contract.

The number of lots we should order should be the amount of money that we are willing to risk divided by the amount of money that we would risk if we order 1 full lot."
  (/ dollar-amount-willing-to-be-risked ;1 pip for 1 lot = $10.  4 decimal points in the data, not 5.
     (* number-of-pips-risked 100000)))	;Evaluates the the dollar-amount being risked, on GBPUSD & EURUSD pairs.

(defun run-simulations-with-r (simulations start-data-point end-data-point)
  "Random entry with r-expectancy for each run"
  (dotimes (number-of-simulations simulations)
    (do ((i start-data-point (1+ i))
	 (length-array end-data-point)
	 (vector-of-orders (make-array 5000 :element-type 'order :fill-pointer 0 :adjustable t))
	 (capital 10000)
	 (minimum-account-balance 10000)
	 (list-of-open-orders nil))
	((>= i (- length-array 7))
	 ;; Do stuff that wraps up a simulation here.
	 )
      (if (null list-of-open-orders)
	  ;; Open order
	  (if (= 1 (random 2))
	      ;; Open buy order.
	      (open-order (openb (aref *vector* i))
			  (- (openb (aref *vector* i))
			     (* 3 (atrb (aref *vector* i))))
			  1
			  (make-instance 'order))
	      ;; Open sell order.
	      (open-order (openb (aref *vector* i))
			  (+ (openb (aref *vector* i))
			     (* 3 (atrb (aref *vector* i))))
			  -1
			  (make-instance 'order)))
	  ;; There is an open order.  Do the following for all open orders.
	  (loop for order in list-of-open-orders
	     with orders-to-remove = nil do
	     ;; Algorithm:
	     ;; pass present high & low & atr-value to function; if functions says so, we will consider the order closed and remove it from the list, else the function will have updated the trailing-stop.
	       (if (eql 'close-this-order
			(handle-order (high (aref *vector* i))
				      (low (aref *vector* i))
				      (atrb (aref *vector* i))))
		   (push order orders-to-remove))
	     finally (loop for closed-order in orders-to-remove do
			  (delete closed-order list-of-open-orders))))
	       

;;;;;;;;;; Functions to make
	  ;; open-order
      ;; Take stuff and return an 'order' object
      ;; Make this decide whether its a buy or sell order?  What if the decision depends on stuff about the history?  Can't we just have the function deal with that itself?  Like with its own interface to the NN?
	  ;; handle-order
      ;; do maintain or close.  if close, then return 'close-this-order.
		      
;;;;;;;;; class: simulation
      ;; this will hold the r-expectancy, number of winners, # of losers, Avg. win, avg lose.  Anything else?
      ;; This happens for every simulation.
