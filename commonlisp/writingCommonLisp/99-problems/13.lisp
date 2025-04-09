;;; problem 13

(defun encode-direct (lst)
  (if (null lst)
      nil
      (let ((result nil)
            (count 1)
            (current (car lst)))
        (dolist (elem (cdr lst) (reverse (cons (if (= count 1)
                                                   current
                                                   (list count current))
                                               result)))
          (if (equal elem current)
              (incf count)
              (progn
                (push (if (= count 1)
                          current
                          (list count current))
                      result)
                (setf current elem)
                (setf count 1)))))))

