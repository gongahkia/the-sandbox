;;; problem 12

(defun decode (lst)
  (apply #'append
         (mapcar (lambda (item)
                   (if (listp item)
                       (make-list (first item) :initial-element (second item))
                       (list item)))
                 lst)))
