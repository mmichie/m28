(defun to-number (x)
  (if (numberp x)
      x
      (string->number (print-value x))))

(defun bubble-sort (lst)
  (let ((n (length lst)))
    (dotimes (i n)
      (dotimes (j (- n i 1))
        (let ((a (nth j lst))
              (b (nth (+ j 1) lst)))
          (when (> (to-number a) (to-number b))
            (setf (nth j lst) b)
            (setf (nth (+ j 1) lst) a)))))
    lst))

;; Test the bubble-sort function
(defvar *original-list* '(5 2 9 1 7 6 3))
(print "Original list:")
(print *original-list*)
(defvar *sorted-list* (bubble-sort *original-list*))
(print "Sorted list:")
(print *sorted-list*)
