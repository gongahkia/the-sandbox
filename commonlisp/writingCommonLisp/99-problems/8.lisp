;;; problem 8

(defun compress (lst)
  (if (null lst)
      nil
      (let ((result (list (car lst))))
        (dolist (elem (cdr lst))
          (unless (equal elem (car result))
            (push elem result)))
        (reverse result))))
