(in-package :cacl)

;;;; Config -------------------------------------------------------------------
(defparameter *undo-limit* 30)


;;;; State --------------------------------------------------------------------
(defvar *running* nil)
(defvar *stack* nil)
(defvar *previous* nil)


;;;; Stack --------------------------------------------------------------------
(defun push! (&rest objects)
  (dolist (o objects)
    (push (if (floatp o)
            (coerce o 'double-float)
            o)
          *stack*)))

(defun pop! ()
  (assert *stack* () "Cannot pop empty stack")
  (pop *stack*))

(defun pop-all! ()
  (prog1 *stack* (setf *stack* nil)))


(defmacro with-args (symbols &body body)
  `(let (,@(iterate (for symbol :in (reverse symbols))
                    (collect `(,symbol (pop!)))))
     ,@body))


;;;; Undo ---------------------------------------------------------------------
(defun save-stack ()
  (unless (eql *stack* (car *previous*))
    (push *stack* *previous*))
  (setf *previous* (subseq *previous* 0 (min (1+ *undo-limit*)
                                             (length *previous*)))))

(defun save-thunk (thunk)
  (push thunk *previous*))

(defun undo ()
  (assert (cdr *previous*) () "Cannot undo any further")
  ;; The first element in *previous* is the current stack, so remove it.
  (pop *previous*)
  (let ((top (car *previous*)))
    (etypecase top
      (list nil)
      (function (funcall top)
                (pop *previous*)))
    (setf *stack* (car *previous*))))


;;;; Math ---------------------------------------------------------------------
(defun cube (number) (* number number number))

(defun factorial (number)
  (iterate (for i :from 1 :to number)
           (multiplying i)))

(defun binomial-coefficient (n k)
  "Return `n` choose `k`."
  ;; See https://en.wikipedia.org/wiki/Binomial_coefficient#Multiplicative_formula
  (iterate (for i :from 1 :to k)
           (multiplying (/ (- (1+ n) i) i))))


;;;; Misc ---------------------------------------------------------------------
(defun sh (command input)
  (declare (ignorable command input))
  #+sbcl
  (sb-ext:run-program (first command) (rest command)
                      :search t
                      :input (make-string-input-stream input))
  #+ccl
  (ccl:run-program (first command) (rest command)
                   :input (make-string-input-stream input))
  #+abcl
  (let ((p (system:run-program (first command) (rest command)
                               :input :stream
                               :output t
                               :wait nil)))
    (write-string input (system:process-input p))
    (close (system:process-input p)))
  #-(or sbcl ccl abcl)
  (error "Not implemented for this Lisp implementation, sorry"))

(defun pbcopy (object)
  (sh '("pbcopy") (structural-string object)))


;;;; Commands -----------------------------------------------------------------
(defgeneric command (symbol))

(defmethod command ((symbol symbol))
  (error "Unknown command ~S" symbol))


(defmacro define-command (symbol-or-symbols args &body body)
  `(progn ,@(iterate (for symbol :in (ensure-list symbol-or-symbols))
                     (collect `(defmethod command ((symbol (eql ',symbol)))
                                 (with-args ,args
                                   ,@body))))))

(defmacro define-simple-command
    (symbols argument-count &optional (lisp-function (first symbols)))
  (let ((args (make-gensym-list argument-count "ARG")))
    `(define-command ,symbols ,args
       (push! (,lisp-function ,@args)))))

(defmacro define-constant-command (symbol value)
  `(define-command ,symbol ()
     (push! ,value)))


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
  (push! (coerce x 'double-float)))
(define-command (clear cl) ()
  (pop-all!))

(define-command (float fl) (x)
  (push! (coerce x 'double-float)))

(define-command range (from below)
  (map nil #'push! (range from below)))

(define-command irange (from to)
  (map nil #'push! (range from (1+ to))))

(define-command pbc (x)
  (pbcopy x)
  (push! x))

(define-command sum ()
  (push! (summation (pop-all!))))

(define-command prod ()
  (push! (product (pop-all!))))

(define-command dup (x)
  (push! x x))

(define-command log (base number)
  (push! (log number base)))

(define-command pop ()
  (pop!))

(define-command version ()
  (print-version))

(define-command (quit q) ()
  (setf *running* nil))

(define-command (swap sw) (x y)
  (push! y x))

(define-command reload ()
  (funcall (read-from-string "ql:quickload") :cacl))

(define-command (reverse rev) ()
  (setf *stack* (reverse *stack*)))

(define-command (hist history) ()
  (let ((*read-default-float-format* 'double-float))
    (flet ((print-entry (e)
             (typecase e
               (list (print (reverse e)))
               (t (print e)))))
      (mapc #'print-entry (reverse *previous*))))
  (terpri))

(define-command (undo un) ()
  (undo)
  (throw :do-not-add-undo-state nil))

(define-command count ()
  (push! (length *stack*)))

(define-command base (n)
  ;; todo figure out how the christ to undo this
  (let ((pb *print-base*)
        (rb *read-base*))
    (save-thunk (lambda ()
                  (setf *print-base* pb
                        *read-base* rb))))
  (setf *print-base* n
        *read-base* n))


;;;; Special Forms ------------------------------------------------------------
(defgeneric special-form (symbol &rest body))

(defmacro define-special-form (symbol arguments &rest body)
  (let ((args (gensym "ARGUMENTS")))
    `(defmethod special-form ((symbol (eql ',symbol)) &rest ,args)
       (destructuring-bind ,arguments ,args
         ,@body))))

(define-special-form quote (value)
  (push! value))


;;;; REPL ---------------------------------------------------------------------
(defmacro with-errors-handled (&body body)
  (with-gensyms (old-stack)
    `(let ((,old-stack *stack*))
       (handler-case (progn ,@body)
         (error (e)
           (format t "~A: ~A~%" (type-of e) e)
           (setf *stack* ,old-stack))))))


(defun read-input ()
  (let ((*read-default-float-format* 'double-float)
        (line (read-line *standard-input* nil :eof nil)))
    (if (eq :eof line)
      (setf *running* nil)
      (read-all-from-string line))))

(defun handle-input (input)
  (with-errors-handled
    (catch :do-not-add-undo-state
      (etypecase input
        (number (push! input))
        (symbol (command input))
        (cons (apply 'special-form input)))
      (save-stack))))

(defun handle-all-input ()
  (mapc #'handle-input (read-input)))


(defun print-stack ()
  (let ((*read-default-float-format* 'double-float))
    (pr (reverse *stack*))))

(defun print-prompt ()
  (princ "? ")
  (force-output))

(defun print-version ()
  (format t "CACL v0.0.0 (~A)~%"
          #+sbcl 'sbcl
          #+ccl 'ccl
          #+ecl 'ecl
          #+abcl 'abcl))


(defun run ()
  (setf *running* t
        *stack* nil
        *previous* (list nil))
  (let ((*package* (find-package :cacl)))
    (iterate (while *running*)
             (progn
               (terpri)
               (print-stack)
               (print-prompt)
               (handle-all-input))))
  (values))

(defun toplevel ()
  (print-version)
  (run))
