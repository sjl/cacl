(in-package :cacl)

(define-command bkb (bytes)
  "Convert bytes to kilobytes."
  (push! (coerce (/ bytes (expt 1024 1)) 'double-float)))

(define-command bmb (bytes)
  "Convert bytes to megabytes."
  (push! (coerce (/ bytes (expt 1024 2)) 'double-float)))

(define-command bgb (bytes)
  "Convert bytes to gigabytes."
  (push! (coerce (/ bytes (expt 1024 3)) 'double-float)))

(define-command btb (bytes)
  "Convert bytes to terabytes."
  (push! (coerce (/ bytes (expt 1024 4)) 'double-float)))

(define-command bpb (bytes)
  "Convert bytes to petabytes."
  (push! (coerce (/ bytes (expt 1024 5)) 'double-float)))

(define-command beb (bytes)
  "Convert bytes to exabytes."
  (push! (coerce (/ bytes (expt 1024 6)) 'double-float)))

(define-command kbb (bytes)
  "Convert kilobytes to bytes."
  (push! (* bytes (expt 1024 1))))

(define-command mbb (bytes)
  "Convert megabytes to bytes."
  (push! (* bytes (expt 1024 2))))

(define-command gbb (bytes)
  "Convert gigabytes to bytes."
  (push! (* bytes (expt 1024 3))))

(define-command tbb (bytes)
  "Convert terabytes to bytes."
  (push! (* bytes (expt 1024 4))))

(define-command pbb (bytes)
  "Convert petabytes to bytes."
  (push! (* bytes (expt 1024 5))))

(define-command ebb (bytes)
  "Convert exabytes to bytes."
  (push! (* bytes (expt 1024 6))))

(define-command c2f (celsius)
  "Convert Celsius to Fahrenheit."
  (push! (+ (* 9/5 celsius) 32)))

(define-command f2c (fahrenheit)
  "Convert Fahrenheit to Celsius."
  (push! (* (- fahrenheit 32) 5/9)))
