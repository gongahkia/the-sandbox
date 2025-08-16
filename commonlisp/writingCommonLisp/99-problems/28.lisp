;;; problem 28

(defun lsort (lst)
  (sort lst #'< :key #'length))

(defun lfsort (lst)
  (let ((frequency (mapcar (lambda (x) (cons x (count x (mapcar #'length lst))))
                           (remove-duplicates (mapcar #'length lst)))))
    (sort lst #'< :key (lambda (x) (cdr (assoc (length x) frequency))))))
