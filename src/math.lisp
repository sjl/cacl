(in-package :cacl)

;;;; Misc ---------------------------------------------------------------------
(defun cube (number) (* number number number))

(defun factorial (number)
  (iterate (for i :from 1 :to number)
           (multiplying i)))

(defun binomial-coefficient (n k)
  "Return `n` choose `k`."
  ;; See https://en.wikipedia.org/wiki/Binomial_coefficient#Multiplicative_formula
  (iterate (for i :from 1 :to k)
           (multiplying (/ (- (1+ n) i) i))))


;;;; Primes -------------------------------------------------------------------
(declaim (ftype (function ((integer 0 #.array-dimension-limit))
                          (simple-array bit (*)))
                sieve%))

(eval-dammit
  (defun sieve% (limit)
    "Return a bit vector of primality for all odd numbers below `limit`."
    (iterate
      (with length = (truncate limit 2))
      (with numbers = (make-array length :initial-element 1 :element-type 'bit))
      (for bit :in-vector numbers :with-index n :from 1)
      (when (= 1 bit)
        (iterate
          (with step = (1+ (* 2 n)))
          (for composite :from (+ n step) :by step :below length)
          (setf (aref numbers composite) 0)))
      (finally
        (setf (aref numbers 0) 0)
        (return numbers)))))

(defun sieve (limit)
  "Return a vector of all primes below `limit`."
  (declare (optimize speed))
  (check-type limit (integer 0 #.array-dimension-limit))
  (if (< limit 2)
    (vector)
    (iterate
      (declare (iterate:declare-variables))
      (if-first-time
        (collect 2 :into result :result-type vector))
      (for bit :in-vector (sieve% limit) :with-index n :from 1)
      (when (= 1 bit)
        (collect (1+ (* 2 n)) :into result :result-type vector))
      (finally (return result)))))


(defun prime-factors (n)
  "Return the prime factors of `n`.

  The result will be a set of primes.  For example:

    (prime-factors 60)
    ; =>
    (2 3 5)

  "
  (let ((result (list)))
    (iterate (while (evenp n)) ; handle 2, the only even prime factor
             (if-first-time
               (push 2 result))
             (setf n (/ n 2)))
    (iterate
      (for i :from 3 :to (sqrt n) :by 2) ; handle odd (prime) divisors
      (iterate (while (dividesp n i))
               (if-first-time
                 (push i result))
               (setf n (/ n i))))
    (when (> n 2) ; final check in case we ended up with a prime
      (push n result))
    (nreverse result)))

(defun prime-factorization (n)
  "Return the prime factorization of `n` as a flat list.

  The result will be a list of primes, with duplicates.  For example:

    (prime-factorization 60)
    ; =>
    (2 2 3 5)

  "
  ;; from http://www.geeksforgeeks.org/print-all-prime-factors-of-a-given-number/
  (let ((result (list)))
    (iterate (while (evenp n)) ; handle 2, the only even prime factor
             (push 2 result)
             (setf n (/ n 2)))
    (iterate
      (for i :from 3 :to (sqrt n) :by 2) ; handle odd (prime) divisors
      (iterate (while (dividesp n i))
               (push i result)
               (setf n (/ n i))))
    (when (> n 2) ; final check in case we ended up with a prime
      (push n result))
    (nreverse result)))

(defun prime-factorization-pairs (n &key include-zeros)
  "Return the prime factorization of `n` as a list of prime/exponent conses.

  The result will be a list of `(prime . exponent)` conses.  If `include-zeros`
  is given, even primes whose exponent turns out to be zero will be included.

  For example:

    (prime-factorization-pairs 60)
    ; =>
    ((2 . 2) (3 . 1) (5 . 1))

  "
  (let ((result (list)))
    (iterate (while (evenp n)) ; handle 2, the only even prime factor
             (else (when include-zeros
                     (push (cons 2 0) result)))
             (if-first-time
               (push (cons 2 1) result)
               (incf (cdar result)))
             (setf n (/ n 2)))
    (iterate
      (for i :from 3 :to (sqrt n) :by 2) ; handle odd (prime) divisors
      (iterate (while (dividesp n i))
               (else (when (and include-zeros (primep i))
                       (push (cons i 0) result)))
               (if-first-time
                 (push (cons i 1) result)
                 (incf (cdar result)))
               (setf n (/ n i))))
    (when (> n 2) ; final check in case we ended up with a prime
      (push (cons n 1) result))
    (nreverse result)))


(defun expmod (base exp m)
  "Return base^exp % m quickly."
  ;; From SICP and
  ;; https://rosettacode.org/wiki/Miller%E2%80%93Rabin_primality_test#Common_Lisp
  ;;
  ;; We want to avoid bignums as much as possible.  This computes (base^exp % m)
  ;; without having to deal with huge numbers by taking advantage of the fact
  ;; that:
  ;;
  ;;     (x * y) % m
  ;;
  ;; is equivalent to:
  ;;
  ;;     ((x % m) * (y % m)) % m
  ;;
  ;; So for the cases where `exp` is even, we can split base^exp into an x and
  ;; y both equal to base^(exp/2) and use the above trick to handle them
  ;; separately.  Even better, we can just compute it once and square it.
  ;;
  ;; We also make it tail recursive by keeping a running accumulator:
  ;;
  ;;    base^exp * acc
  (labels
      ((recur (base exp acc)
         (cond
           ((zerop exp) acc)
           ((evenp exp)
            (recur (rem (square base) m)
                   (/ exp 2)
                   acc))
           (t
            (recur base
                   (1- exp)
                   (rem (* base acc) m))))))
    (recur base exp 1)))


(defun factor-out (n factor)
  "Factor the all the `factor`s out of `n`.

  Turns `n` into:

    factor^e * d

  where `d` is no longer divisible by `n`, and returns `e` and `d`.

  "
  (iterate (for d :initially n :then (/ d factor))
           (for e :initially 0 :then (1+ e))
           (while (dividesp d factor))
           (finally (return (values e d)))))

(defun miller-rabin-prime-p (n &optional (k 11))
  "Return whether `n` might be prime.

  If `t` is returned, `n` is probably prime.
  If `nil` is returned, `n` is definitely composite.

  "
  ;; https://rosettacode.org/wiki/Miller%E2%80%93Rabin_primality_test#Common_Lisp
  (cond
    ((< n 2) nil)
    ((< n 4) t)
    ((evenp n) nil)
    (t (multiple-value-bind (r d)
           (factor-out (1- n) 2)
         (flet ((strong-liar-p (a)
                  (let ((x (expmod a d n)))
                    (or (= x 1)
                        (iterate (repeat r)
                                 (for y :initially x :then (expmod y 2 n))
                                 (when (= y (1- n))
                                   (return t)))))))
           (iterate (repeat k)
                    (for a = (random-range-exclusive 1 (1- n)))
                    (always (strong-liar-p a))))))))

(defun brute-force-prime-p (n)
  "Return (slowly) whether `n` is prime."
  (cond
    ((or (= n 0) (= n 1)) nil)
    ((= n 2) t)
    ((evenp n) nil)
    (t (iterate (for divisor :from 3 :to (sqrt n))
                (when (dividesp n divisor)
                  (return nil))
                (finally (return t))))))


;;; We precompute a bit vector of the primality of the first few odd prime
;;; numbers to make checking primes faster.

(defconstant +precomputed-primality-limit+ 10000000)

(defparameter *precomputed-primality-bit-vector*
  (sieve% +precomputed-primality-limit+))

(deftype precomputed-primality-bit-vector ()
  `(simple-array bit (,(truncate +precomputed-primality-limit+ 2))))

(deftype integer-with-precomputed-primality ()
  `(integer 0 (,+precomputed-primality-limit+)))

(defun-inline precomputed-prime-p% (n)
  (declare (optimize speed (debug 0) (safety 1))
           (type integer-with-precomputed-primality n)
           (type precomputed-primality-bit-vector *precomputed-primality-bit-vector*))
  (not (zerop (aref *precomputed-primality-bit-vector* (truncate n 2)))))


(defun primep (n)
  "Return (less slowly) whether `n` is prime."
  (cond
    ;; short-circuit a few edge/common cases
    ((< n 2) nil)
    ((= n 2) t)
    ((evenp n) nil)
    ((< n +precomputed-primality-limit+) (precomputed-prime-p% n))
    (t (miller-rabin-prime-p n))))

(defun compositep (n)
  "Return whether `n` is composite."
  (and (not (= 1 n))
       (not (primep n))))

(defun coprimep (a b)
  (= 1 (gcd a b)))




;;;; Commands -----------------------------------------------------------------

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

(define-command (mean average) ()
  "Pop the entire stack, compute the mean, and push the result."
  (let* ((xs (pop-all!))
         (n (length xs)))
    (when (zerop n)
      (error "Cannot compute mean of empty sequence."))
    (push! (/ (summation xs) n))))

(define-command median ()
  "Pop the entire stack, compute the median, and push the result."
  (let* ((xs (sort (pop-all!) #'<))
         (n (length xs)))
    (when (zerop n)
      (error "Cannot compute median of empty sequence."))
    (push! (if (oddp n)
             (nth (floor n 2) xs)
             (/ (+ (nth (1- (/ n 2)) xs)
                   (nth (/ n 2) xs))
                2)))))

(define-command log (base x)
  (push! (log x base)))

(define-command ln (x)
  (push! (log x)))

(define-command pm (x y)
  "Push both results of x ± y."
  (push! (- x y))
  (push! (+ x y)))

(define-command primep (x)
  (push! (primep x)))

(define-command coprimep (x y)
  (push! (coprimep x y)))

(define-command compositep (x)
  (push! (compositep x)))

(define-command (prime-factors factors) (x)
  (dolist (f (prime-factors x))
    (push! f)))

(define-command (prime-factorization factorization fz) (x)
  (dolist (f (prime-factorization x))
    (push! f)))

(define-command pf (x)
  "Print x as a nicely-formatted number."
  (let ((sign (if (minusp x) "-" ""))
        (x (abs x)))
    (multiple-value-bind (ipart fpart) (ftruncate x)
      (let ((ipart (round ipart))
            (fpart (if (zerop fpart)
                     ""
                     (subseq (format nil "~F" fpart) 1))))
        (format t "~A~:D~A" sign ipart fpart))))
  (values))
