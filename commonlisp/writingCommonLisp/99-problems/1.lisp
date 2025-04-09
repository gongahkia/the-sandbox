;;; problem 1

(defun my-last (last-item)
    (if last-item
        (last last-item) ; t form, evaluated if list exists and is not empty
      nil) ; nil form, evaluated if list is empty
)

;;; execution on sample input

(print (my-last '(a b c d)))
