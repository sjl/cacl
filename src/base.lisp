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

(defmacro with-read-only-args (symbols &body body)
  `(let (,@(iterate (for i :from 0)
                    (for symbol :in (reverse symbols))
                    (collect `(,symbol (nth ,i *stack*)))))
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
          * Numbers and characters are pushed onto the stack.~@
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


(defmacro define-command% (symbol args read-only &body body)
  (multiple-value-bind (forms declarations documentation)
      (alexandria:parse-body body :documentation t)
    `(progn
       (defmethod command ((symbol (eql ',symbol)))
         (,(if read-only 'with-read-only-args 'with-args) ,args
           ,@declarations
           ,@forms))
       (defmethod command-documentation ((symbol (eql ',symbol)))
         ,(or documentation "No documentation provided"))
       (pushnew ',symbol *commands*))))

(defmacro define-command (symbol-or-symbols args &body body)
  (let ((read-only (member '&read-only args))
        (args (remove '&read-only args)))
    `(progn ,@(iterate
                (for symbol :in (alexandria:ensure-list symbol-or-symbols))
                (collect `(define-command% ,symbol ,args ,read-only ,@body))))))

(defmacro define-simple-command
    (symbols argument-count &optional (lisp-function (first symbols)))
  (let ((args (alexandria:make-gensym-list argument-count "ARG")))
    `(define-command ,symbols ,args
       (push! (,lisp-function ,@args)))))

(defmacro define-constant-command (symbol value)
  `(define-command ,symbol ()
     (push! ,value)))


;;;; Commands/IO --------------------------------------------------------------
(define-command pbc (&read-only x)
  "Copy the top element of the stack to the system clipboard.

  The item will remain on the stack.

  "
  (pbcopy x))

(define-command pbp ()
  "Push the contents of the system clipboard onto the stack as a string."
  (push! (pbpaste)))


;;;; Commands/Stack -----------------------------------------------------------
(define-command (clear cl) ()
  "Clear the entire stack."
  (pop-all!))

(define-command (print p) (&read-only item)
  "Print `item`.  It will remain on the stack."
  (princ (structural-string item))
  (terpri)
  (force-output))

(define-command (pprint pp) (&read-only item)
  "Pretty print `item`.  It will remain on the stack."
  (pprint item)
  (terpri)
  (force-output))

(define-command (dup d) (x)
  "Duplicate the top element of the stack."
  (push! x x))

(define-command pop ()
  "Pop the top element of the stack."
  (pop!))

(define-command (length len) (item)
  (push! (length item)))

(define-command (swap x) (x y)
  "Exchange the top two elements of the stack."
  (push! y x))

(define-command (reverse rev) ()
  "Reverse the stack."
  (setf *stack* (reverse *stack*)))

(define-command (hist history) ()
  (let ((*read-default-float-format* 'double-float))
    (flet ((print-entry (e)
             (typecase e
               (list (print-stack e))
               (t (prin1 e) (terpri)))))
      (mapc #'print-entry (reverse *previous*))))
  (terpri))

(define-command count ()
  "Push the length of the stack."
  (push! (length *stack*)))

(define-command (undo un) ()
  (undo)
  (throw :do-not-add-undo-state nil))


;;;; Commands/System ----------------------------------------------------------
(define-command doc (symbol)
  "Print the documentation for the symbol at the top of the stack."
  (format t "~A: ~A~%" symbol (command-documentation symbol)))

(define-command help ()
  "Print some basic help information."
  (print-help))

(define-command commands ()
  "Print a list of available commands."
  (print-commands))

(define-command reload ()
  "Reload the entire CACL system from Quicklisp."
  (funcall (read-from-string "ql:quickload") :cacl))

(define-command (quit q) ()
  "Quit CACL."
  (setf *running* nil))

(define-command version ()
  "Print the version and host Lisp."
  (print-version))

(define-command nop ()
  "Do nothing.")


;;;; Commands/Misc ------------------------------------------------------------
(define-command char-code (char)
  (push! (char-code char)))

(define-command code-char (code)
  (push! (code-char code)))


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
  (alexandria:with-gensyms (old-stack)
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
        ((or number string character) (push! input))
        (symbol (command input))
        (cons (apply 'special-form input)))
      (save-stack))))

(defun handle-all-input ()
  (mapc #'handle-input (read-input)))


(defun print-stack (&optional (stack *stack*))
  (write-char #\()
  (let ((*read-default-float-format* 'double-float))
    (format t "~{~A~^ ~}" (reverse stack)))
  (write-char #\))
  (terpri)
  (force-output))

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


;;;; Command Line -------------------------------------------------------------
(adopt:define-string *documentation*
  "CACL is a TUI RPN calculator.~@
  ~@
  Documentation about the command-line options to the CACL binary follows.  ~
  For information about how to use CACL itself type help when running in interactive mode.")

(defparameter *o-help*
  (adopt:make-option 'help
    :help "display help and exit"
    :long "help"
    :short #\h
    :reduce (constantly t)))

(defparameter *o-rcfile*
  (adopt:make-option 'rcfile
    :help "path to the custom initialization file (default ~/.caclrc)"
    :long "rcfile"
    :parameter "PATH"
    :initial-value "~/.caclrc"
    :reduce #'adopt:last))

(defparameter *o-no-rcfile*
  (adopt:make-option 'no-rcfile
    :result-key 'rcfile
    :help "disable loading of any rcfile"
    :long "no-rcfile"
    :reduce (constantly nil)))

(defparameter *o-interactive*
  (adopt:make-option 'interactive
    :result-key 'mode
    :help "run in interactive mode (the default)"
    :long "interactive"
    :short #\i
    :initial-value 'interactive
    :reduce (constantly 'interactive)))

(defparameter *o-batch*
  (adopt:make-option 'batch
    :result-key 'mode
    :help "run in batch processing mode"
    :long "batch"
    :short #\b
    :reduce (constantly 'batch)))

(defparameter *o-inform*
  (adopt:make-option 'inform
    :help "print informational message at startup (the default)"
    :long "inform"
    :initial-value t
    :reduce (constantly t)))

(defparameter *o-no-inform*
  (adopt:make-option 'no-inform
    :result-key 'inform
    :help "suppress printing of informational message at startup"
    :long "no-inform"
    :reduce (constantly nil)))


(defparameter *ui*
   (adopt:make-interface
     :name "cacl"
     :usage "[OPTIONS]"
     :summary "A TUI RPN calculator written and customizable in Common Lisp."
     :help *documentation*
     :contents
     (list *o-help*
           *o-rcfile*
           *o-no-rcfile*
           *o-interactive*
           *o-batch*
           *o-inform*
           *o-no-inform*)))


(defun toplevel ()
  ;; ccl clobbers the pprint dispatch table when dumping an image, no idea why
  (set-pprint-dispatch 'hash-table 'losh:pretty-print-hash-table)
  (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
    (cond ((gethash 'help options)
           (adopt:print-help-and-exit *ui*))
          (arguments
           (cerror "Ignore them" "Unrecognized command-line arguments: ~S" arguments)))
    (when (gethash 'inform options)
      (print-version))
    (when-let ((rc (gethash 'rcfile options)))
      (load rc :if-does-not-exist nil))
    (run)))


