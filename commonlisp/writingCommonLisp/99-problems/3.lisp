;;; problem 3

(defun element-at (lst k)
  (if (and lst (>= k 1))
      (if (= k 1)
          (first lst)
          (element-at (rest lst) (- k 1)))
      nil))

;;; execution on sample input

(print (element-at '(a b c d e) 3))
