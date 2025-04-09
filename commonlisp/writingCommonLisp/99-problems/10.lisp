;;; problem 10

(defun encode (lst)
  (mapcar (lambda (sublist) (list (length sublist) (car sublist)))
          (pack lst)))
