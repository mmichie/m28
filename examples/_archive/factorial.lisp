(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(print "Factorial of 5:")
(print (factorial 5))

(print "Factorials from 0 to 10:")
(do ((i 0 (+ i 1)))
    ((> i 10) nil)
  (print (concatenate 'string "Factorial of " (number->string i) ": " (number->string (factorial i)))))
