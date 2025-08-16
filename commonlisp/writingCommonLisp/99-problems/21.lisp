;;; problem 21

(defun insert-at (elem lst k)
  (append (subseq lst 0 (1- k)) (list elem) (subseq lst (1- k))))
