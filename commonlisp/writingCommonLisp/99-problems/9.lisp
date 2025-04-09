;;; problem 9

(defun pack (lst)
  (if (null lst)
      nil
      (let ((result nil)
            (current (list (car lst))))
        (dolist (elem (cdr lst) (reverse (cons (reverse current) result)))
          (if (equal elem (car current))
              (push elem current)
              (progn
                (push (reverse current) result)
                (setf current (list elem))))))))
