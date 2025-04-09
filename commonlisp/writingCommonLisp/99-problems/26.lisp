;;; problem 26

(defun combination (k lst)
  (cond ((zerop k) (list nil))
        ((null lst) nil)
        (t (append (mapcar (lambda (x) (cons (car lst) x))
                           (combination (1- k) (cdr lst)))
                   (combination k (cdr lst))))))
