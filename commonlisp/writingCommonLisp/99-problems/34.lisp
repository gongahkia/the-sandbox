;;; problem 34

(defun totient-phi (m)
  (let ((count 0))
    (loop for i from 1 to (1- m) do
          (when (coprime i m)
            (incf count)))
    count))
