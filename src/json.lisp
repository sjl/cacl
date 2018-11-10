(in-package :cacl)

(defun string-to-json (string)
  (let ((yason:*parse-json-booleans-as-symbols* t)
        (yason:*parse-json-arrays-as-vectors* t))
    (yason:parse string)))

(defun json-to-string (json &key indent)
  (with-output-to-string (s)
    (yason:encode json (if indent (yason:make-json-output-stream s) s))))


(define-command (from-json fj) (string)
  (etypecase string
    (string (push! (string-to-json string)))))

(define-command (to-json tj) (json)
  (etypecase json
    (json (push! (json-to-string json)))))


