(in-package :cacl)

;;;; Config -------------------------------------------------------------------
(defparameter *undo-limit* 30)


;;;; State --------------------------------------------------------------------
(defvar *running* nil)
(defvar *stack* nil)
(defvar *previous* nil)
(defvar *commands* nil)


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


;;;; Help ---------------------------------------------------------------------
(defun first-letter (command)
  (let ((ch (aref (symbol-name command) 0)))
    (if (alphanumericp ch)
      ch
      #\!)))

(defun partition-commands (commands)
  (mapcar (lambda (letter-and-commands)
            (sort (second letter-and-commands) #'string<))
          (sort (hash-table-contents (group-by #'first-letter commands))
                #'char< :key #'first)))

(defun print-version ()
  (format t "CACL v0.0.0 (~A)~%"
          #+sbcl 'sbcl
          #+ccl 'ccl
          #+ecl 'ecl
          #+abcl 'abcl))

(defun print-help ()
  (terpri)
  (format t "CACL is an RPN calculator written in Common Lisp.~@
          ~@
          The current stack is displayed above the prompt (the top is at the right).~@
          ~@
          Forms are read from standard input with the standard Common Lisp READ function.~@
          This means you can put multiple things on one line if you want, like this:~@
          ~%    1 2 +~@
          ~@
          What happens when a form is read depends on the form:~@
          ~@
          * Numbers are pushed onto the stack.~@
          * Symbols run commands.~@
          * Quoted forms are pushed onto the stack.~@
          ~@
          Type `commands` for a list of available commands.~@
          ~@
          To get help for a particular command, push its symbol onto the stack~@
          and run the `doc` command:~@
          ~%    'float doc~@
          "))

(defun print-commands ()
  (terpri)
  (format t "AVAILABLE COMMANDS:~@
             ~(~{~{~A~^ ~}~%~}~)~%"
          (partition-commands *commands*)))


;;;; Commands -----------------------------------------------------------------
(defgeneric command (symbol))

(defmethod command ((symbol symbol))
  (error "Unknown command ~S" symbol))


(defgeneric command-documentation (symbol))

(defmethod command-documentation (object)
  (flet ((friendly-type (object)
           (let ((type (type-of object)))
             (if (consp type) (first type) type))))
    (error "Cannot retrieve documentation for ~S ~S"
           (friendly-type object) object)))

(defmethod command-documentation ((symbol symbol))
  (error "Unknown command ~S" symbol))


(defmacro define-command% (symbol args &body body)
  (multiple-value-bind (forms declarations documentation)
      (parse-body body :documentation t)
    `(progn
       (defmethod command ((symbol (eql ',symbol)))
         (with-args ,args
           ,@declarations
           ,@forms))
       (defmethod command-documentation ((symbol (eql ',symbol)))
         ,(or documentation "No documentation provided"))
       (pushnew ',symbol *commands*))))

(defmacro define-command (symbol-or-symbols args &body body)
  `(progn ,@(iterate
              (for symbol :in (ensure-list symbol-or-symbols))
              (collect `(define-command% ,symbol ,args ,@body)))))

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


(define-command (clear cl) ()
  "Clear the entire stack."
  (pop-all!))

(define-command (float fl) (x)
  "Coerce the top of the stack to a DOUBLE-FLOAT."
  (push! (coerce x 'double-float)))

(define-command range (from below)
  (map nil #'push! (range from below)))

(define-command irange (from to)
  (map nil #'push! (range from (1+ to))))

(define-command pbc (x)
  "Copy the top element of the stack to the system clipboard.

  SBCL only for now, sorry."
  (pbcopy x)
  (push! x))

(define-command sum ()
  "Pop the entire stack, add everything together, and push the result."
  (push! (summation (pop-all!))))

(define-command prod ()
  "Pop the entire stack, multiply everything together, and push the result."
  (push! (product (pop-all!))))

(define-command dup (x)
  "Duplicate the top element of the stack."
  (push! x x))

(define-command log (base number)
  (push! (log number base)))

(define-command nop ()
  "Do nothing.")

(define-command pop ()
  "Pop the top element of the stack."
  (pop!))

(define-command version ()
  "Print the version and host Lisp."
  (print-version))

(define-command (quit q) ()
  "Quit CACL."
  (setf *running* nil))

(define-command (swap sw) (x y)
  "Swap the top two elements of the stack."
  (push! y x))

(define-command reload ()
  "Reload the entire CACL system from Quicklisp."
  (funcall (read-from-string "ql:quickload") :cacl))

(define-command (reverse rev) ()
  "Reverse the stack."
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
  "Push the length of the stack."
  (push! (length *stack*)))

(define-command doc (symbol)
  "Print the documentation for the symbol at the top of the stack."
  (format t "~A: ~A~%" symbol (command-documentation symbol)))

(define-command help ()
  "Print some basic help information."
  (print-help))


(define-command commands ()
  "Print a list of available commands."
  (print-commands))

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

(define-command base10 ()
  "Set the print base and read base for numbers to base 10."
  (let ((pb *print-base*)
        (rb *read-base*))
    (save-thunk (lambda ()
                  (setf *print-base* pb
                        *read-base* rb))))
  (setf *print-base* 10
        *read-base* 10))


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
