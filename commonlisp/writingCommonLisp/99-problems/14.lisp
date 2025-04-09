;;; problem 14

(defun dupli (lst)
  (apply #'append (mapcar (lambda (x) (list x x)) lst)))
