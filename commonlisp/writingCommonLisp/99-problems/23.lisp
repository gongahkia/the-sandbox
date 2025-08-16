;;; problem 23

(defun rnd-select (lst n)
  (if (zerop n)
      nil
      (let ((index (random (length lst))))
        (cons (nth index lst)
              (rnd-select (remove-at lst (1+ index)) (1- n))))))
