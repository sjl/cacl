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

(define-simple-command (!) (x) factorial)
(define-simple-command (*) (x y))
(define-simple-command (+) (x y))
(define-simple-command (-) (x y))
(define-simple-command (/) (x y))
(define-simple-command (abs) (x))
(define-simple-command (acos) (x))
(define-simple-command (asin) (x))
(define-simple-command (atan) (y))
(define-simple-command (atan2) (y x) atan)
(define-simple-command (ceiling ceil) (x))
(define-simple-command (choose) (n k) binomial-coefficient)
(define-simple-command (cos) (x))
(define-simple-command (cs) (x) -)
(define-simple-command (cube) (x))
(define-simple-command (denom) (x) denominator)
(define-simple-command (expt ex) (base power))
(define-simple-command (floor) (x))
(define-simple-command (gcd) (x y))
(define-simple-command (lcm) (x y))
(define-simple-command (mod) (x modulus))
(define-simple-command (numer) (n) numerator)
(define-simple-command (rat) (x) rationalize)
(define-simple-command (rec recip 1/) (x) /)
(define-simple-command (rem) (x divisor))
(define-simple-command (round) (x))
(define-simple-command (sin) (x))
(define-simple-command (sqrt) (x))
(define-simple-command (square sq) (x))
(define-simple-command (tan) (x))
(define-simple-command (truncate trunc tr) (x) truncate)

(define-command (float fl f) (x)
  "Coerce the top of the stack to a `double-float`."
  (push! (coerce x 'double-float)))

(define-command 1in (p)
  "Convert a probability to `1 in X` form and push `X`."
  (let ((p (rationalize p)))
    (push! (coerce (/ (denominator p) (numerator p))
                   'double-float))))

(define-command range (from below)
  "Push an exclusive range of numbers.  The highest number will be at the top of the stack."
  (loop for x :from from :below below :do (push! x)))

(define-command irange (from to)
  "Push an inclusive range of numbers.  The highest number will be at the top of the stack."
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

(define-command log (base x)
  (push! (log x base)))

(define-command ln (x)
  (push! (log x)))

(define-command kb (bytes)
  "Convert bytes to kilobytes."
  (push! (coerce (/ bytes (expt 1024 1)) 'double-float)))

(define-command mb (bytes)
  "Convert bytes to megabytes."
  (push! (coerce (/ bytes (expt 1024 2)) 'double-float)))

(define-command gb (bytes)
  "Convert bytes to gigabytes."
  (push! (coerce (/ bytes (expt 1024 3)) 'double-float)))

(define-command tb (bytes)
  "Convert bytes to terabytes."
  (push! (coerce (/ bytes (expt 1024 4)) 'double-float)))

(define-command pb (bytes)
  "Convert bytes to petabytes."
  (push! (coerce (/ bytes (expt 1024 5)) 'double-float)))

(define-command eb (bytes)
  "Convert bytes to exabytes."
  (push! (coerce (/ bytes (expt 1024 6)) 'double-float)))
