;;; problem 19

(defun rotate (lst n)
  (let* ((len (length lst))
         (n (mod n len)))
    (append (subseq lst n) (subseq lst 0 n))))
