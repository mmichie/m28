(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(print "Fibonacci sequence up to 10:")
(do ((i 0 (+ i 1)))
    ((> i 10) nil)
  (print (fib i)))
