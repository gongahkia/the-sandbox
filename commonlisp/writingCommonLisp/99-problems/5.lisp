;;; problem 5

(defun my-reverse (lst)
  (labels ((rev (lst acc)
            (if (null lst)
                acc
                (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))
