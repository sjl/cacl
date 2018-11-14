(in-package :cacl)

(defun decode-json% (string)
  (let ((yason:*parse-json-booleans-as-symbols* t)
        (yason:*parse-json-arrays-as-vectors* t))
    (yason:parse string)))

(defun encode-json% (json &key indent)
  (with-output-to-string (s)
    (yason:encode json (if indent (yason:make-json-output-stream s) s))))


(define-command (decode-json dj) (string)
  (etypecase string
    (string (push! (decode-json% string)))))

(define-command (encode-json ej) (object)
  (etypecase object
    ((or hash-table vector null number (member yason:true yason:false) string)
     (push! (encode-json% object)))))


