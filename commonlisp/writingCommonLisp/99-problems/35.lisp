;;; problem 35

(defun prime-factors (n)
  (labels ((factorize (n divisor factors)
             (cond ((= n 1) (reverse factors))
                   ((zerop (mod n divisor))
                    (factorize (/ n divisor) divisor (cons divisor factors)))
                   ((> (* divisor divisor) n)
                    (reverse (cons n factors)))
                   (t (factorize n (+ divisor (if (= divisor 2) 1 2)) factors)))))
    (factorize n 2 nil)))
