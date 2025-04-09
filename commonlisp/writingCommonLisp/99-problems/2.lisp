;;; problem 2

;; car => first element of list
;; cdr => rest of the list besides the first

(defun my-but-last (input-list)
    (if (endp input-list) ; predicate
        nil ; true form
        (if (endp (cdr input-list)) ; false form => predicate
            nil ; true form
            (list (cadr (last input-list)) (car (last input-list))))) ; false form
)

;;; execution on sample input

(print (my-but-last '(a b c d)))
