(load "./src/package.lisp")
(load "./src/main.lisp")

(let ((score (flx:score "buffer-file-name" "bfn")))
  (print score)
  (unless (equal score '(237 0 7 12))
    (print "Error testing `buffer-file-name`")
    (quit :unix-status 1)))
