# Random-entry
 *  This program simulates a random-entry trading system.  The decision to make a buy-or-sell order is based on a random choice.
 *  Only one trade occurs at a time.
 *  Each trade has a trailing stop-loss of 3 * ATR(20)
# Use of this program
1. Load the program into your lisp system and enter the package.
2. Run `(process-data "name-of-the-OHLC-data-file" *array*)`
3. Run `(run-simulations-with-r number-of-simulations number-that-is->-20 number-that-is-<-length-of-*array*)`
   After that you will be able to review the results using 
4. `(array-to-csv *array-foo* "name-of-your-favourite-CSV-file")`
   Just open the CSV-file in your favourite spreadsheet software.
