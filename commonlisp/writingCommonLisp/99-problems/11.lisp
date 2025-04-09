;;; problem 11

(defun encode-modified (lst)
  (mapcar (lambda (sublist)
            (let ((len (length sublist)))
              (if (= len 1)
                  (car sublist)
                  (list len (car sublist)))))
          (pack lst)))
