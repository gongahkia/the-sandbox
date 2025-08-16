;;; problem 7

(defun my-flatten (lst)
  (cond ((null lst) nil)
        ((listp (car lst))
         (append (my-flatten (car lst)) (my-flatten (cdr lst))))
        (t (cons (car lst) (my-flatten (cdr lst))))))
