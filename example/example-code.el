;;; difference-of-squares.el --- Difference of Squares (exercism)

;;; Commentary:

;;    Interesting exercise that is quite simple to solve if you can
;;    use standard mapcar with a generated list of numbers. Obviously,
;;    the `number-sequence' call won't work with extremely large
;;    numbers.
;;
;;    But my goal, I think, was to make something parallel and
;;    readable.

;;; Code:

(defun square (n)
  "Returns the square of a number."
  (* n n))

(defun sum (list-of-nums)
  "Like the + function, but takes a list."
  (apply '+ list-of-nums))

(defun squares (list-of-nums)
  "Returns LIST-OF-NUMS where each element is squared."
  (mapcar 'square list-of-nums))

(defun square-of-sums (n)
  "Returns the square of all sums of n with all natural numbers
lower than n."
  (square (sum (number-sequence 1 n))))

(defun sum-of-squares (n)
  "Sums the square of n with the square of all natural number
lower than n."
  (sum (squares (number-sequence 1 n))))

(defun difference (n)
  "Difference between the square of sums and the sums of squares."
  (- (square-of-sums n) (sum-of-squares n)))
(provide 'difference-of-squares)
;;; difference-of-squares.el ends here
