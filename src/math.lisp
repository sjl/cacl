(in-package :cacl)

(defun cube (number) (* number number number))

(defun factorial (number)
  (iterate (for i :from 1 :to number)
           (multiplying i)))

(defun binomial-coefficient (n k)
  "Return `n` choose `k`."
  ;; See https://en.wikipedia.org/wiki/Binomial_coefficient#Multiplicative_formula
  (iterate (for i :from 1 :to k)
           (multiplying (/ (- (1+ n) i) i))))



(define-constant-command e (exp 1.0d0))
(define-constant-command pi pi)
(define-constant-command tau tau)

(define-simple-command (!) 1 factorial)
(define-simple-command (*) 2)
(define-simple-command (+) 2)
(define-simple-command (-) 2)
(define-simple-command (/) 2)
(define-simple-command (abs) 1)
(define-simple-command (acos) 1)
(define-simple-command (asin) 1)
(define-simple-command (atan) 1)
(define-simple-command (atan2) 2 atan)
(define-simple-command (ceiling ceil) 1)
(define-simple-command (choose) 2 binomial-coefficient)
(define-simple-command (cos) 1)
(define-simple-command (cs) 1 -)
(define-simple-command (cube) 1)
(define-simple-command (denom) 1 denominator)
(define-simple-command (expt ex) 2)
(define-simple-command (floor) 1)
(define-simple-command (gcd) 2)
(define-simple-command (lcm) 2)
(define-simple-command (mod) 2)
(define-simple-command (numer) 1 numerator)
(define-simple-command (rat) 1 rationalize)
(define-simple-command (rec recip) 1 /)
(define-simple-command (rem) 2)
(define-simple-command (round) 1)
(define-simple-command (sin) 1)
(define-simple-command (sqrt) 1)
(define-simple-command (square sq) 1)
(define-simple-command (tan) 1)
(define-simple-command (truncate trunc tr) 1 truncate)

(define-command (float fl) (x)
  "Coerce the top of the stack to a DOUBLE-FLOAT."
  (push! (coerce x 'double-float)))

(define-command 1in (p)
  "Convert a probability to `1 in X` form and push `X`."
  (let ((p (rationalize p)))
    (push! (coerce (/ (denominator p) (numerator p))
                   'double-float))))

(define-command range (from below)
  (loop for x :from from :below below :do (push! x)))

(define-command irange (from to)
  (loop for x :from from :to to :do (push! x)))

(define-command base (n)
  "Set the print base and read base for numbers to the top element of the stack.

  For example, to switch to reading and displaying numbers in binary:

    2 base

  To switch back to base 10 you can run the command again, but you'll need to
  write the 10 in the base you've chosen!  It's often easer to `undo`, or use
  the provided `base10` command.

  "
  (let ((pb *print-base*)
        (rb *read-base*))
    (save-thunk (lambda ()
                  (setf *print-base* pb
                        *read-base* rb))))
  (setf *print-base* n
        *read-base* n))

(define-command bits (x)
  "Pop the top of the stack and print its binary representation."
  (unless (typep x '(integer 0 *))
    (error "BITS requires a nonnegative integer."))
  (format t "~v,'0,' ,4:B~%"
          (let ((chunks (ceiling (integer-length x) 4)))
            (+ (* 4 chunks) ; actual bits
               (1- chunks))) ; comma chars
          x))

(define-command hex (x)
  "Pop the top of the stack and print its hex representation."
  (unless (typep x '(integer 0 *))
    (error "HEX requires a nonnegative integer."))
  (format t "~X~%" x))

(define-command base10 ()
  "Set the print base and read base for numbers to base 10."
  (let ((pb *print-base*)
        (rb *read-base*))
    (save-thunk (lambda ()
                  (setf *print-base* pb
                        *read-base* rb))))
  (setf *print-base* 10
        *read-base* 10))

(define-command sum ()
  "Pop the entire stack, add everything together, and push the result."
  (push! (summation (pop-all!))))

(define-command prod ()
  "Pop the entire stack, multiply everything together, and push the result."
  (push! (product (pop-all!))))

(define-command log (base number)
  (push! (log number base)))

