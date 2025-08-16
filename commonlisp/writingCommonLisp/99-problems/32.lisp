;;; problem 32

(defun gcd (a b)
  (if (zerop b)
      a
      (gcd b (mod a b))))
