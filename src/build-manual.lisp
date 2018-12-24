(ql:quickload :cacl)

(with-open-file (stream "build/cacl.1" :direction :output :if-exists :supersede)
  (adopt:print-manual cacl::*ui* :stream stream))
