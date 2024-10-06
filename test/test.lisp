(load "./flx.lisp")

(let ((score (flx:score "buffer-file-name" "bfn")))
  (print score)
  (terpri)
  (unless (equal score '(237 0 7 12))
    (print (format nil "Error testing `buffer-file-name`: ~a" score))
    (quit :unix-status 1)))
