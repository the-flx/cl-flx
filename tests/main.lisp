(defpackage flx/tests/main
  (:use :cl
   :flx
        :rove))

(in-package :flx/tests/main)

(deftest test-target-1
  (testing "test score: buffer-file-name"
    (ok (equal (flx:score "buffer-file-name" "bfn") '(237 0 7 12)))))
