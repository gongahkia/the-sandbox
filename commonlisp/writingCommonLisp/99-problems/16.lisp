;;; problem 16

(defun drop (lst n)
  (labels ((drop-rec (lst idx)
             (cond ((null lst) nil)
                   ((= idx n) (drop-rec (cdr lst) 1))
                   (t (cons (car lst) (drop-rec (cdr lst) (+ idx 1)))))))
    (drop-rec lst 1)))
