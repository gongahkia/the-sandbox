;;; problem 17

(defun split (lst n)
  (labels ((split-rec (lst n acc)
             (if (or (null lst) (zerop n))
                 (values (reverse acc) lst)
                 (split-rec (cdr lst) (- n 1) (cons (car lst) acc)))))
    (multiple-value-bind (first second) (split-rec lst n nil)
      (list first second))))
