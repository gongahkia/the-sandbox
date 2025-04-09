;;; problem 31

(defun is-prime (n)
  (cond ((< n 2) nil)
        ((= n 2) t)
        ((evenp n) nil)
        (t (loop for i from 3 to (isqrt n) by 2
                 never (zerop (mod n i))))))
