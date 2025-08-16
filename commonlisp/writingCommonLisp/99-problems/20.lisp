;;; problem 20

(defun remove-at (lst k)
  (append (subseq lst 0 (1- k)) (subseq lst k)))
