;;; problem 22

(defun range (a b)
  (if (< a b)
      (loop for i from a to b collect i)
      (loop for i from a downto b collect i)))
