;;; problem 15

(defun repli (lst n)
  (apply #'append (mapcar (lambda (x) (make-list n :initial-element x)) lst)))
